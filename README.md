# EDTaupeEye
Elite Dangerous Tool (using Tobii Eye trackers to play with Elite  Dangerous) From **Galactic 911**

**Project in progress** [12345678**9**-]

## **Abstract**   

- **EDTaupeEye** is an interface that allows you to use the **[Tobii Eye Trackers](https://gaming.tobii.com/product/eye-tracker-5)** to play **[Elite Dangerous](https://www.elitedangerous.com)**. 
The overlay splits the screen into multiple areas that can be activated individually using gazing 
and blinking to simulate a mouse click.

- **EdTaupeEye** reads the *Custom.3.0.binds* file in order to know the keyboard shortcuts. He can add his 
own shortcuts to access all the functions available in Elite Dangerous. The tool keeps a history 
of changes it has made to the file.

- **EdTaupeEye** also scans the *Status.json* file to contextually adapt the functions that can be displayed.

## **Install**
Please follow these recommendations during installation

1) *Elite Dangerous* state off
2) Install and run *EDTaupeEye*. You may encounter the following cases

  - Message : **Elite Dangerous commands settings : Please choose CUSTOM.**
Stop *EDTaupeEye* and restart *Elite Dangerous*, go to Commands settings and Choose Custom or modify a keyboard entry save and quit *Elite Dangerous*. Restart *EDTaupeEye*.

  - Message : **Delete one of the joystick or mouse inputs.**
Stop *EDTaupeEye* and restart *Elite Dangerous*, go to Commands settings, find all inputs that use two joystick signals and delete one of the two. Restart *EDTaupeEye*. 

In general, *EDTaupeEye* needs to use *Custom.3.0.binds* file where for each entry one of the two possibilities must be available. *EdTaupeEye* can add its own values. 
In this case, it will make a backup of the *Custom.3.0.binds* file before saving its own modifications.

If you make an in-game modification of the bindings, then you must relaunch *EDTaupeEye*, because the *Custom.3.0.binds* file is only read when the application is launched.

## **Use of voice software in addition**

You can use any voice software with *EDTaupeEye*, but we recommend that you use the *RaoulFumier* software available on *GitHub* for french language (https://github.com/MaxiDonkey/RaoulFumier). 
This software has the advantage of allowing many commands to be executed simultaneously.
This software is only available for the French language and does not require any configuration to be used with *Elite Dangerous*. 
It also allows you to dictate your messages to other commanders.

## **Overview**
[Piloting the ship](https://github.com/MaxiDonkey/EDTaupeEye/tree/master/img/edte_img0mini.png)

[Access to Elite functions](https://github.com/MaxiDonkey/EDTaupeEye/tree/master/img/edte_img1mini.png)

[Galaxy map manipulations ](https://github.com/MaxiDonkey/EDTaupeEye/tree/master/img/edte_img2mini.png)

[Menus manipulations](https://github.com/MaxiDonkey/EDTaupeEye/tree/master/img/edte_img3mini.png)

[Using keyboard](https://github.com/MaxiDonkey/EDTaupeEye/tree/master/img/edte_img4mini.png)

## **Video**
[![Demo](http://www.maxidonkey.com/EDTaupeEye.mp4)](http://www.maxidonkey.com/EDTaupeEye.mp4)




