# shunda_tec06
This provides text (to stdout) and GUI data collection from a Shunda Tec-06 battery tester, as described at https://www.sevarg.net/2018/01/14/tec-06-serial-battery-tester-review/ and https://www.sevarg.net/2018/01/20/reverse-engineering-tec-06-serial/

Note that it relies on a non-standard high-speed serial data rate, so far the only device that I've found that supports this is a genuine (not a counterfeit) FTDI FT232.

If it is built as a console program (e.g. using the included makefile) or the GUI variant is invoked with a port on the commandline (e.g. /dev/ttyUSB0) then it provides text output which can be redirected to a file. If it is run as a GUI application (no commandline parameter) then it provides graphical output but has no provision to save either a graph or captured text to a file.

Overall layout has been heavily influenced by the appearance of the original Chinese-language program, but as yet there is very limited backend functionality.

![Screenshot](tec06/tec-06.png)

At this point I thought I'd somehow managed to blow my tester up, but it turns out that it's quite sensitive to the length (and presumably quality) of the USB lead.

I've had to add some really crappy code, since it appears that the tester's firmware (as of v6.34) is inclined to swap a couple of message fields towards the end of a test: I speculate that it was originally written either for a charger or for a more complex device which could do a discharge-charge cycle. I'm not particulary happy with it, but it's run for a week while I've tested my stock of cells.

TODO: any form of data export from the GUI variant, either as a portable file (CSV etc.) or an image of the graph.
