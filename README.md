[![Build Status](https://travis-ci.com/StanfordAHA/lake.svg?branch=master)](https://travis-ci.com/StanfordAHA/lake)

# lake
lake is a framework for generating synthesizable memory modules from a high-level behavioral specification and widely-available memory macros.

todo: transcribe notes

## lake.components

**lake.components.lang**: high level memory-construct language
  Present a formal spec of memory behavior that can be transformed into synthesizable rtl control logic. 
  * Need to talk to the HLS guys about a reasonable solution -> hopefully peak can support this
  
**lake.components.lex**: pattern analyzer
  High level analysis can be performed on the patterns to determine performance under various memory macro parameterizations.
 
**lake.components.parmem**: parametric memory generator
  Provide a highly configurable memory logic that can interface to the provided memory macro as well as 
  interface to the control logic. This is the middle man so-to-speak. 
  
we would like to incorporate some level of formal analysis to make guarantees about functionality and performance against the high-level model

where is the interface?

standard control logic interface to feature-rich memory corev
or
control logic + memory transactions to bare memory macro
