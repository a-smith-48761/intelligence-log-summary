### Intelligence Log Summarizer

A project to parse the "session history" log for an interviewer produced by the UNICOM Intelligence CATI system (as deployed at Ipsos UK).

The log file has the following format:

```
dd/mm/yyyy, hh:mm:ss	<recordid>	<duration>	<outcome>	<initial characters of notes added if any>
```
Separators are tabs, except between date and time. Record ID may be empty, as may the notes be.

Special cases:
* An outcome of "Interviewer_Wait" is time spent waiting for a call to be connected
* We identify subcases of outcomes based on presence of specific markers in the notes:
  * "HUDI" is a subcase of both the CallbackAnotherTime and Refused categories, which we count separately.
  * "Screening" is also a subcase of CallbackAnotherTime that's tracked separately.

Outcome categories we track are: `Answering Machine, CallbackAnotherTime, Appointment*, *Refus*, Abandon*, Complet*, NoAnswer/Busy, OOQ*`.
Any other categories are reported in an "Other" category along with a brief list.

Average timings and standard deviation for each category are presented along with total numbers.
