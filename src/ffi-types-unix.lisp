(in-package :cserial-port)

(include "sys/ioctl.h")
(include "termios.h")

(constant (B0 "B0"))
(constant (B50 "B50"))
(constant (B75 "B75"))
(constant (B110 "B110"))
(constant (B134 "B134"))
(constant (B150 "B150"))
(constant (B200 "B200"))
(constant (B300 "B300"))
(constant (B600 "B600"))
(constant (B1200 "B1200"))
(constant (B1800 "B1800"))
(constant (B2400 "B2400"))
(constant (B4800 "B4800"))
(constant (B9600 "B9600"))
(constant (B19200 "B19200"))
(constant (B38400 "B38400"))
(constant (B57600 "B57600"))
(constant (B115200 "B115200"))
(constant (B230400 "B230400"))
(constant (b460800 "B460800"))
(constant (B500000 "B500000"))
(constant (B576000 "B576000"))
(constant (B921600 "B921600"))
(constant (B1000000 "B1000000"))
(constant (B1152000 "B1152000"))
(constant (B1500000 "B1500000"))
(constant (B2000000 "B2000000"))
(constant (B2500000 "B2500000"))
(constant (B3000000 "B3000000"))
(constant (B3500000 "B3500000"))
(constant (B4000000 "B4000000"))

;;c_iflag
(constant (IGNBRK "IGNBRK"))
(constant (BRKINT "BRKINT"))
(constant (IGNPAR "IGNPAR"))
(constant (PARMRK "PARMRK"))
(constant (INPCK "INPCK"))
(constant (ISTRIP "ISTRIP"))
(constant (INLCR "INLCR"))
(constant (IGNCR "IGNCR"))
(constant (ICRNL "ICRNL"))
(constant (IXON "IXON"))
(constant (IXANY "IXANY"))
(constant (IXOFF "IXOFF"))

;;c_oflag
(constant (OPOST "OPOST"))
(constant (ONLCR "ONLCR"))
(constant (OCRNL "OCRNL"))
(constant (ONOCR "ONOCR"))
(constant (ONLRET "ONLRET"))
(constant (OFILL "OFILL"))

;;c_cflag
(constant (CSIZE "CSIZE"))
(constant (CSTOPB "CSTOPB"))
(constant (CREAD "CREAD"))
(constant (PARENB "PARENB"))
(constant (PARODD "PARODD"))
(constant (HUPCL "HUPCL"))
(constant (CLOCAL "CLOCAL"))

(constant (CS5 "CS5"))
(constant (CS6 "CS6"))
(constant (CS7 "CS7"))
(constant (CS8 "CS8"))

;;c_lflag
(constant (ISIG "ISIG"))
(constant (ICANON "ICANON"))
(constant (ECHO "ECHO"))
(constant (ECHOE "ECHOE"))
(constant (ECHOK "ECHOK"))
(constant (ECHONL "ECHONL"))
(constant (NOFLSH "NOFLSH"))
(constant (TOSTOP "TOSTOP"))
(constant (IEXTEN "IEXTEN"))


;;c_cc
(constant (VINTR "VINTR"))
(constant (VQUIT "VQUIT"))
(constant (VERASE "VERASE"))
(constant (VKILL "VKILL"))
(constant (VEOF "VEOF"))
(constant (VMIN "VMIN"))
(constant (VEOL "VEOL"))
(constant (VTIME "VTIME"))
(constant (VSTART "VSTART"))
(constant (VSTOP "VSTOP"))
(constant (VSUSP "VSUSP"))

;;tcsetattr optional_actions
(constant (TCSANOW "TCSANOW"))
(constant (TCSADRAIN "TCSADRAIN"))
(constant (TCSAFLUSH "TCSAFLUSH"))

(constant (NCCS "NCCS"))

(ctype tcflag-t "tcflag_t")
(ctype cc-t "cc_t")
(ctype speed-t "speed_t")
(ctype pid-t "pid_t")

(cstruct termios "struct termios"
  "The termios structure"
  (iflag "c_iflag"  :type tcflag-t)
  (oflag "c_oflag"  :type tcflag-t)
  (cflag "c_cflag"  :type tcflag-t)
  (lflag "c_lflag"  :type tcflag-t)
  (cc "c_cc"        :type cc-t :count NCCS))

;;ioctl

(constant (TIOCMGET "TIOCMGET"))
(constant (TIOCMSET "TIOCMSET"))
(constant (TIOCMBIC "TIOCMBIC"))
(constant (TIOCMBIS "TIOCMBIS"))
(constant (FIONREAD "FIONREAD"))

(bitfield modem-state
  ((:dsr "TIOCM_DSR"))
  ((:dtr "TIOCM_DTR"))
  ((:rts "TIOCM_RTS"))
  ((:cts "TIOCM_CTS"))
  ((:dcd "TIOCM_CAR"))
  ((:ring "TIOCM_RNG")))
