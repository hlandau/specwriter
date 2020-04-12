#!/bin/sh
set -e; exec guile --fresh-auto-compile --no-auto-compile -L "$(dirname "$0")/../../src" -L "$(dirname "$0")/../../src/schspec-iso" -L "$(dirname "$0")/../../src/schspec-regdef" -L "$(dirname "$0")/../../src/schspec-fig" -L "$(dirname "$0")/../../dep" -s "$0" "$@" #!#
;; vim: filetype=scheme fdm=marker
(use-modules (ice-9 match) (ice-9 format) (ice-9 popen) (sxml2) (specwriter) (schspec-iso) (schspec-regdef) (schspec-fig) (gcrypt hash) (gcrypt base16))

;; Terminology                                                              {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dt pci-device "PCI device" "A PCI, PCI-X or PCI Express device.")
(dt pci-function "PCI function" "A PCI, PCI-X or PCI Express function forming part of a "pci-device".")
(dt message "message" "A sequence of one or more octets.")
(dt unipipe "unipipe" "A unidirectional FIFO down which "messages" are sent.")
(dt bipipe "bipipe" "A combination of two "unipipes", one in each direction.")
(dt pipe-peer "pipe peer" "The entity connected to the other side of a "unipipe" or "bipipe".")
(dt reqpipe "reqpipe" "A "bipipe" in which request "messages" are sent in one direction, and response "messages" in response to those request "messages" are subsequently sent in the other direction by the "pipe-peer".")
(dt notifypipe "notifypipe" "A "unipipe" in which asynchronous event notifications are sent in one direction, and at arbitrary times.")
(dt naturally-aligned "naturally aligned" "Property wherein the integral address of a register is divisible by its size in bytes.")



;; Register Definitions                                                     {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define registers (regblock->sxml `(regblock
  (mnem CTLR)
  (title "Controller Registers")
  (reg CAP
       (title "Controller Capabilities")
       (offset #x00)
       (width 64)
       (access ro)
       (field (0 15) MQES
              (access ro)
              (title "Maximum Queue Entries Supported")
              (desc "This field indicates the maximum individual queue size that the controller supports. This value applies to each of the I/O Submission and I/O Completion Queues that host software may create. This is a zeroes-based value. The minimum value is 1, indicating two entries."))
       (field 16 CQR
              (access ro)
              (title "Contiguous Queues Required")
              (desc "This field is set to 1 if the controller requires that I/O Submission Queues and I/O Completion Queues are required to be physically contiguous. This field is cleared to zero if the controller supports I/O Submission Queues and I/O Completion Queues that are not physically contiguous. If this field is set to 1, then the Physically Contiguous bit (CDW11.PC) in the Create I/O Submission Queue and Create I/O Completion Queue commands shall be set to 1."))
       (field 17 AMS_WRRU
              (title "Arbitration Mechanism Supported — Weighted Round Robin with Urgent")
              (desc "This field is bit significant and indicates the optional arbitration mechanisms supported by this controller. If a bit is set to 1, then the corresponding arbitration mechanism is supported by the controller. Refer to § XXX for arbitration details."))
       (field 18 AMS_VS
              (title "Arbitration Mechanism Supported — Vendor Specific")
              (desc "..."))
       (field (24 31) TO
              (title "Timeout")
              (desc "This is the worst case time that host software shall wait for the controller to become ready (the larger of the time required when CSTS.RDY is set to 1 or CSTS.RDY is cleared to 0) after a power-on or reset (§ XXX). This worst case time may be experienced after an unclean shutdown; typical times are expected to be much shorter. This field is in 500 millisecond units."))
       (field (32 35) DSTRD
              (title "Doorbell Stride")
              (desc "Each Submission Queue and Completion Queue Doorbell register is 32 bits in size. This register indicates the stride between doorbell registers. The stride is specified as 2**(2+DSTRD) in bytes. A value of 0 indicates a stride of 4 bytes, where the doorbell registers are packed without reserved space between each register. Refer to § XXX."))
       (field 37 CSS_NVM
              (title "Command Sets Supported — NVM Command Set")
              (desc "This field indicates the I/O Command Sets that the controller supports. A minimum of one command set shall be supported. The field is bit significant. If a bit is set to 1, then the corresponding I/O Command Set is supported. If a bit is cleared to 0, then the corresponding I/O Command Set is not supported."))
       (field (48 51) MPSMIN
              (title "Memory Page Size Minimum")
              (desc "This field indicates the minimum host memory page size that the controller supports. The minimum memory page size is 2**(12+MPSMIN). The host shall not configure a memory page size in CC.MPS that is smaller than this value."))
       (field (52 55) MPSMAX
              (title "Memory Page Size Maximum")
              (desc "This field indicates the maximum host memory page size that the controller supports. The maximum memory page size is 2**(12+MPSMAX). The host shall not configure a memory page size in CC.MPS that is larger than this value.")))
  (reg VS
       (title "Version")
       (offset #x08)
       (width 32)
       (access ro)
       (field (0 15) MNR
              (title "Minor Version Number")
              (reset 0))
       (field (16 31) MJR
              (title "Major Version Number")
              (reset 1)))
  (reg IVMS
       (title "Interrupt Mask Set")
       (offset #x0C)
       (width 32)
       (access rw1s)
       (reset 0)
       (desc "This field is bit significant. If a 1 is written to a bit, then the corresponding interrupt vector is masked. Writing a 0 to a bit has no effect. When read, this field returns the current interrupt mask value within the ocntroller (not the value of this register). If a bit has a value of 1, then the corresponding interrupt vector is masked. If a bit has a value of 0, then the corresponding interrupt vaector is not masked."))
  (reg IVMC
       (title "Interrupt Mask Clear")
       (offset #x10)
       (width 32)
       (access rw1c)
       (reset 0)
       (desc "This field is bit significant. If a 1 is written to a bit, then the corresponding interrupt vector is unmasked. Writing a 0 to a bit has no effect. When read, this field returns the current interrupt mask value within the controller (not the value of this register). If a bit has a value of 1, then the corresponding interrupt vector is masked. If a bit has a value of 0, then the corresponding interrupt vector is not masked."))
  (reg CC
       (title "Controller Configuration")
       (offset #x14)
       (width 32)
       (reset 0)
       (field 0 EN
              (title "Enable")
              (access rw)
              (desc "When set to 1, the controller shall process commands based on Submission Queue Tail doorbell writes. When cleared to 0, then the controller shall not process commands nor post completion queue entries to Completion Queues. When this field transitions from 1 to 0, the controller is reset (referred to as a Controller Reset). The reset deletes all I/O Submission Queues and I/O Completion Queues, resets the Admin Submission Queue and Completion Queue, and brings the hardware to an idle state. The reset does not affect PCI Express registers nor the Admin Queue registers (AQA, ASQ or ACQ). All other controller registers defined in this section and internal controller state (e.g. feature values defined in § XXX that are not persistent across power states) are reset to their default values. The controller shall ensure that there is no data loss for commands that have had corresponding completion queue entries posted to an I/O Completion Queue prior to the reset operation. Refer to § XXX for reset details.

                 When this field is cleared to 0, the CSTS.RDY bit is cleared to 0 by the controller once the controller is ready to be re-enabled. When this field is set to 1, the controller sets CSTS.RDY to 1 when it is ready to process commands. Setting this field from a 0 to a 1 when CSTS.RDY is a 1, or setting this field from a 1 to a 0 when CSTS.RDY is 0, has undefined results. The Admin Queue registers (AQA, ASQ and ACQ) shall only be modified when EN is cleared to 0."))
      (field (4 6) CSS
             (title "I/O Command Set Selected")
             (access rw)
             (desc "This field specifies the I/O Command Set that is selected for use for the I/O Submission Queues. Host software shall only select a supported I/O Command Set, as indicated in CAP.CSS. This field shall only be changed when the controller is disabled (CC.EN is cleared to 0). The I/O Command Set selected shall be used for all I/O Submission Queues.")
             (enum 0 (title "NVM Command Set")))
      (field (7 10) MPS
             (title "Memory Page Size")
             (access rw)
             (desc "This field indicates the host memory page size. The memory page size is 2**(12+MPS). Thus, the minimum host memory page size is 4KiB and the maximum host memory page size is 128MiB. The value set by host software shall be a supported value as indicated by the CAP.MPSMAX and CAP.MPSMIN fields. This field describes the value used for PRP entry size."))
      (field (11 13) AMS
             (title "Arbitration Mechanism Selected")
             (access rw)
             (desc "This field selects the arbitration mechanism to be used. This value shall only be changed when EN is cleared to 0. Host software shall only set this field to supported arbitration mechanisms indicated in CAP.AMS. If this field is set to an unsupported value, the behaviour is undefined.")
             (enum 0 (title "Round Robin"))
             (enum 1 (title "Weighted Round Robin with Urgent"))
             (enum 7 (title "Vendor Specific")))
      (field (14 15) SHN
             (title "Shutdown Notification")
             (access rw)
             (desc "This field is used to initiate shutdown processing when a shutdown is occurring (i.e., a power down condition is expected). For a normal shutdown notification, it is expected that the controller is given time to process the shutdown notification. For an abrupt shutdown notification, the host may not wait for shutdown processing to complete before power is lost.

                   This field should be written by host software prior to any power down condition and prior to any change of the PCI power management state. It is recommended that this field also be written prior to a warm reboot. To determine when shutdown processing is complete, refer to CSTS.SHST. Refer to § XXX for additional shutdown processing details.")
             (enum 0 (title "No Notification/No Effect"))
             (enum 1 (title "Normal Shutdown Notification"))
             (enum 2 (title "Abrupt Shutdown Notification")))
      (field (16 19) IOSQES
             (title "I/O Submission Queue Entry Size")
             (access rw)
             (desc "This field defines the I/O Submission Queue entry size that is used for the selected I/O Command Set. The required and maximum values for this field are specified in the Identify Controller data structure in Figure XXX for each I/O Command Set. The value is in bytes and specified as a power of two (2**n)."))
      (field (20 23) IOCQES
             (title "I/O Completion Queue Entry Size")
             (access rw)
             (desc "This field defines the I/O Completion Queue entry size that is used for the selected I/O Command Set. The required and maximum values for this field are specified in the Identify Controller data structure in Figure XXX for each I/O Command Set. The value is in bytes and specified as a power of two (2**n).")))
    (reg CSTS
         (title "Controller Status")
         (offset #x1C)
         (width 32)
         (reset 0)
         (access ro)
         (field 0 RDY
                (title "Ready")
                (desc "This field is set to 1 when the controller is ready to accept Submission Queue Tail doorbell writes after CC.EN is set to 1. This field shall be cleared to 0 when CC.EN is cleared to 0. Commands shall not be submitted to the controller until this field is set to 1 after the CC.EN bit is set to 1. Failure to follow this requirement produces undefined results. Host software shall wait a minimum of CAP.TO seconds for this field to be set to 1 after setting CC.EN to 1 from a previous value of 0."))
         (field 1 CFS
                (title "Controller Fatal Status")
                (desc "This field is set to 1 when a fatal controller error occurred that could not be communicated in the appropriate Completion Queue. This field is cleared to 0 when a fatal controller error has not occurred. Refer to § XXX."))
         (field (2 3) SHST
                (title "Shutdown Status")
                (desc "This field indicates the status of shutdown processing that is initiated by the host setting the CC.SHN field. To start executing commands on the controller after a shutdown operation, a reset (CC.EN cleared to 0) is required. If host software submits commands to the controller without issuing a reset, the behaviour is undefined.")
                (enum 0 (title "Normal Operation (no shutdown task has been requested"))
                (enum 1 (title "Shutdown processing occurring"))
                (enum 2 (title "Shutdown processing complete"))))
    (reg AQA
         (title "Admin Queue Attributes")
         (offset #x24)
         (width 32)
         (reset 0)
         (field (0 11) ASQA
                (title "Admin Submission Queue Size")
                (access rw)
                (desc "Defines the size of the Admin Submission Queue in entries. Refer to § XXX. The minimum size of the Admin Submission Queue is two entries. The maximum size of the Admin Submission Queue is 4096 entries. This is a zeroes-based value."))
         (field (26 27) ACQS
                (title "Admin Completion Queue Size")
                (access rw)
                (desc "Defines the size of the Admin Completion Queue in entries. Refer to § XXX. The minimum size of the Admin Completion Queue is two entries. The maximum size of the Admin Completion Queue is 4096 entries. This is a zeroes-based value.")))
    (reg ASQ
         (title "Admin Submission Queue Base Address")
         (offset #x28)
         (width 64)
         (reset impl)
         (field (12 63) ASQB
                (title "Admin Submission Queue Base")
                (desc "Indicates the 64-bit physical address for the Admin Submission Queue. This address shall be memory page aligned (based on the value in CC.MPS). All Admin commands, including creation of I/O Submission Queues and I/O Completion Queues, shall be submitted to this queue. For the definition of Submission Queues, refer to § XXX.")))
    (reg ACQ
         (title "Admin Completion Queue Base Address")
         (offset #x30)
         (width 64)
         (reset impl)
         (field (12 63) ACQB
                (title "Admin Completion Queue Base")
                (desc "Indicates the 64-bit physical address for the Admin Completion Queue. This address shall be memory page aligned (based on the value in CC.MPS). All completion queue entries for the commands submitted to the Admin Submission Queue shall be posted to this Completion Queue. This queue is always associated with interrupt vector 0. For the definition of Completion Queues, refer to § XXX.")))
    (reg SQyTDBL
         (title "Submission Queue y Tail Doorbell")
         (offset (+ #x1000 (* 2 y (lsh 4 CAP.DSTRD))))
         (width 32)
         (reset 0)
         (field (0 15) SQT
                (title "Submission Queue Tail")
                (access rw)
                (desc "Indicates the new value of the Submission Queue Tail entry pointer. This value shall overwrite any previous Submission Queue Tail entry pointer value provided. The difference between the last SQT write and the current SQT write indicates the number of commands added to the Submission Queue. Note that Submission Queue rollover needs to be accounted for.")))
    (reg CQyHDBL
         (title "Completion Queue y Head Doorbell")
         (offset (+ #x1000 (* (+ 1 (* 2 y)) (lsh 4 CAP.DSTRD))))
         (width 32)
         (reset 0)
         (field (0 15) CQH
                (title "Completion Queue Head")
                (access rw)
                (desc "Indicates the new value of the Completion Queue Head entry pointer. This value shall overwrite any previous Completion Queue Head value provided. The difference between the last CQH write and the current CQH entry pointer write indicates the number of entries that are now available for re-use by the controller in the Completion Queue. Note that Completion Queue rollover needs to be accounted for."))))))


;; Main Document                                                            {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (document) (doc (docctl (docinfo (doctitle "Some Document"))) (docbody (doccontent (docfront) (docproper (docmain
  (§ "Scope")
  (§ "References")
  (apply § (cons "Definitions" (map xlate-termdef sw-terms)))

  (§ "Abstract Definition of Host-Controller Interface"
     (p "This specification defines an interface between a host and a storage controller. The interface describes the interactions between the host and the storage controller:")

     (ul
       (li "The controller provides a set of registers to the host, which the host "may" get and set.")
       (li "The host provides a memory subsystem to the controller, by which the controller "may" retrieve or store blocks of data, which are identified by a locator value.")
       (li "The controller "may" generate event notifications to the host."))

     (p "This interface may be more precisely defined by the following set of service primitives:")

     (ul
       (li (procn "DeviceRegGet")"(r: Register) → (value) | error;")
       (li (procn "DeviceRegSet")"(r: Register, value) → ok | error;")
       (li (procn "HostMemGet")"(l: Locator, numBytes: ui) → (dataBlock) | error;")
       (li (procn "HostMemSet")"(l: Locator, dataBlock) → ok | error;")
       (li (procn "HostNotify")"(param) → ok | error;")))

  (with-id 'spatial (§ "Spatial Realization of Register Sets"
     (p "A register set may be spatially realized by assigning each register an offset X from some undefined base B in a linear space. The offset of the register is then denoted B+X. The unit of the linear space is one byte. Each register is one or more bytes in size, and registers "shall-not" overlap in the space. Registers "shall" be "naturally-aligned" within the space. The endianness of the realised registers with regard to the byte-addressed memory system "must" be defined.")
     (p "This realization does not preclude other modes of realization of a register set from being defined.")))

  (§i "FOO Informative" (p "Hello."))
  (§ "Functional Model and State Machines"
     (§ "Pipes"
        (§ "Abstract Definition of Operations on Pipes"
           (p "A "unipipe" supports the following service primitives:")
           (ul
             (li "Enqueue(message: []byte) → ok | error;")))
        (§ "Register-based realization of pipes"
           (§ "General"
             (p "A "unipipe" is realised as the following set of registers:")
             (ul
               (li "Base address register")
               (li "Size register")
               (li "Doorbell register"))
             (p "A "bipipe" is realized as two realized "unipipes"."))
           (§ "Realization of the Enqueue Operation")))
     (§ "Device Model"
        (figure "Some Figure"
          (dot-raw "digraph X { graph[splines=ortho]; edge[dir=none]; node[shape=box, fontname=\"Helvetica, 'Neue Haas Grotesk Text Pro', 'Neue Haas Grotesk Text', 'TeX Gyre Heros', 'Helvetica Neue', Arial, sans-serif\"]; Book -> ToC; ToC [label=\"Table of Contents\"]; Book -> Preface; Book -> Chapter; Chapter [penwidth=2]; Book -> Index; Chapter -> Figure; Chapter -> Section; Preface -> Outline; Preface -> IntroText; IntroText [label=\"Introductory Text\"]; Preface -> Figure; Figure [penwidth=2,style=dashed]; Section [penwidth=2]; Index [style=dashed]; Preface[style=dashed]; }"))
        ))

  (§ "Register Definitions"
     registers))

  (docannex
    (§a "PCI Binding"
        (§ "General"
          (p "This annex defines a binding of the host-controller interface as a "pci-device"."))
        (§ "Device Requirements"
           (p "A "pci-device" "shall" be a PCI, PCI-X or PCI Express device.")
           (p "A "pci-device" "shall" have at least one "pci-function" which complies with the requirements for "pci-functions" complying with this specification ("(clauseref 'pci-function)"). Each such "pci-function" constitutes a completely separate instance of the interface defined by this specification. The "pci-device" "may" also have other, unrelated "pci-functions".")
           (p "If a "pci-device" has multiple "pci-functions" (whether or not those "pci-functions" are "pci-functions" implementing this specification), it "shall" implement the ACS capability on all "pci-functions"."))

        (with-id 'pci-function (§ "Function Requirements"
           (§ "General"
             (p "A "pci-function" which is part of a PCI Express device "shall" be a PCI Express or Root Complex Integrated Endpoint, and "shall" not be a Legacy Endpoint. (Note, however, that a PCI or PCI-X device "may" be attached to a PCIe host via a bridge, so a PCIe host system could still see a Legacy Endpoint implementing this specification, which is valid. This requirement only pertains to native PCI Express implementations.)")
             (p "The "pci-function"'s configuration space "shall" have a class code of TBD and "shall" indicate a programming interface of TBD.")
             (p "The "pci-function" "shall" expose a 32-bit or 64-bit memory BAR as BAR0/1. The "pci-function" "shall" support configuring this BAR as a 64-bit memory BAR, "shall" support configuring this BAR as a 32-bit memory BAR, and "shall-not" allow this BAR to be configured as an I/O space BAR. This BAR is henceforth known as BAR0, and offsets from the start of the memory space claimed by it "shall" be denoted BAR0+X.")
             (p "The register set "shall" be mapped at BAR0+0 using the spatial realization defined in "(clauseref 'spatial)". The little endian representation "shall" be used. Any extra space in the BAR after the defined register set is reserved for vendor-specific registers.")
             (p "The BAR "should" have the smallest power-of-two size adequate to contain the register set and any vendor-specific registers.")
             (p "The "pci-function" "shall" implement MSI-X.")
             (p "The "pci-function" "shall" implement AER.")
             (p "The "pci-function" "shall-not" initiate I/O space requests.")
             (p "The "pci-function" "shall-not" initiate or support locked transactions."))

           (§ "Concrete Realization of Service Primitives"
              (§ "Registers"
                 (p "The "(procn "DeviceRegGet")" and "(procn "DeviceRegSet")" service primitives are realized as PCI memory space reads and writes respectively to BAR0+X, where X is the offset of the register in the spatial realization. Except where otherwise specified, such memory reads and writes "shall" be register-aligned and "shall" have sizes equal to the size of th register; otherwise their effects are undefined."))
              (§ "Accommodation of 32-bit systems"
                 (p "Because not all hosts may be able to generate 64-bit memory reads and writes, all 64-bit registers "may" be accessed as two aligned 32-bit accesses to the two halves of these registers. For writes, the first half (the half with the lower memory address) of all registers "shall" be written last in order to ensure correct operation of doorbells. Writing the second half of a doorbell register registers the bits in that half; writing the first half of a doorbell register registers the bits in that half and triggers the doorbell."))
              (§ "Host memory access"
                 (p "The "(procn "HostMemGet")" and "(procn "HostMemSet")" service primitives are realised as PCI memory space reads and writes respectively, generated by the "pci-function" as a bus master (for PCI or PCI-X) or to the "pci-device"'s upstream port (for PCIe). The locator argument is realized as a 64-bit unsigned integer representing a memory address. Accesses "shall" be "naturally-aligned", meaning that said address "shall" be divisible by the size of the access in bytes. (The PCIe specification determines whether 64-bit or 32-bit transactions are generated; namely, a 64-bit transaction is only generated when the high 32 bits of the memory address are nonzero.")
                 (p "The "pci-device" "shall-not" generate PCI memory space reads or writes except as a result of the invocation of the "(procn "HostMemGet")" and "(procn "HostMemSet")" service primitives, respectively, and the addresses of such accesses "shall" have locators strictly matching those passed as arguments to those service primitives.")
                 (note "Since it is in turn a requirement that the "(procn "HostMemGet")" and "(procn "HostMemSet")" service primitives are not invoked other than where specified by this specification, this means that a compliant device "shall-not" initiate PCI memory space reads or writes other than when and as specified by this specification.")
                 (p "The "pci-device" "shall" support arbitrary 64-bit locator values as passed to it by the host, and "shall-not" assume that such integers "shall" always fall within certain subsets of the space [0, 2**64).")

                 (p "If the "(procn "HostMemGet")" or "(procn "HostMemSet")" service primitives are invoked while the "pci-function" is in a state where it is not allowed to issue memory space reads and writes (for example, because bus mastering is disabled in the "pci-function"'s control register), the behaviour is undefined."))
              (§ "Notifications"
                 (p "The "(procn "HostNotify")" service primitive is realised as an MSI-X interrupt. The parameter is realised as an unsigned integer expressing the MSI-X vector number, where 0 is the first vector."))
              (§ "Definition"
                 (ul
                   (li (procn "DeviceRegGet")"(r: ui) → (value: ui) | error;")
                   (li (procn "DeviceRegSet")"(r: ui, value: ui) → ok | error;")
                   (li (procn "HostMemGet")"(l: u64, numBytes: ui) → (dataBlock) | error;")
                   (li (procn "HostMemSet")"(l: u64, dataBlock) → ok | error;")
                   (li (procn "HostNotify")"(param: u32) → ok | error;")))))))

    (§a "Embedded Binding"
        (§ "General"
          (p "This annex defines a binding of the host-controller interface within the memory space of an embedded CPU. It is intended to be useful for applications where use of the HCI as an embedded SoC peripheral is desired and where a PCIe hierarchy is not present.")
          (p "The register set "shall" be mapped at some device-specific base B+0 using the spatial realization defined in "(clauseref 'spatial)"."))
        (§ "Concrete Realization of Service Primitives"
           (p "The "(procn "DeviceRegGet")" and "(procn "DeviceRegSet")" service primitives are realized as host memory reads and writes respectively to B+X, where X is the offset of the register in the spatial realization. The endianness used is system specific. Except where otherwise specified, such memory reads and writes "shall" be register-aligned and "shall" have sizes equal to the size of the register; otherwise their effects are undefined.")
           (p "The "(procn "HostMemGet")" and "(procn "HostMemSet")" service primitives are realized as memory reads and writes to the same host memory space. The locator argument is realized as a 64-bit unsigned integer representing a host memory address. Accesses "shall" be "naturally-aligned", meaning that said address "shall" be divisible by the size of the access in bytes.")
           (p "The "(procn "HostNotify")" service primitive is realized in a device-specific manner.")))))))))


;;                                                                          {{{1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define top (xml-top (number-things (document))))

(sxml->xml top)
