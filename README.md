# Test Your Haskell Knowledge

This application was built as an assessment for the Haskell programming training given by Emurgo Academy.
The idea behind this project assessment was to challenge students to put in practice what they had learned during the training.<br/>
Being inspired by Emurgo learning platform, I came up with the idea of building a terminal application that tests user's knowledge on Monad.<br/><br/>
HOW IT WORKS<br/><br/>
The application's tui is rendered using the Brick library, and questions and answers are loaded from a .json file used as a data source and configuration file.
The application is quite simple, so there's no user registration process. The test is anonymous and the result of the test is not saved.<br/><br/>
The following screen content can be configured using the .json file:<br/>
	<ul>
    <li>title of the test</li>
	  <li>home screen summary</li> 
	  <li>result screen overview</li>
    <li>result scores</li>
 </ul><br/>
SCREENS<br/><br/>
Home<br/>

![image](https://user-images.githubusercontent.com/108430505/223427518-1f3b6ad0-a142-4dd9-9801-b31e8286aa17.png)

Test<br/>

![image](https://user-images.githubusercontent.com/108430505/223427666-bbf3ed49-9d64-4cbf-a72b-ee0463d16552.png)

Result<br/>

![image](https://user-images.githubusercontent.com/108430505/223428397-fe66916f-d668-454f-90c1-3a12e064dfa6.png)

CREDITS<br/>

none of this would have been possible without the help of this user guide on brick library and ofcourse the training given by Emurgo Academy
https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst


