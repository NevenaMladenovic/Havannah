# Havannah
Havannah is two player strategy connection board game. The board is a hexagonal grid shaped in the form of a hexagon - 8 hexes per side. 

This is a console application made with Common Lisp.

User enters his moves in format (letter number)

![Potez](https://user-images.githubusercontent.com/39527815/85962545-94802f00-b9b1-11ea-8cf5-d0236995cef8.png)

The game is over when one of the players makes one of three winning structures - ring, bridge, fork.

A ring is a loop around one or more cells, whether those cells are occupied or empty

![EndGame - Ring](https://user-images.githubusercontent.com/39527815/85962972-d27e5280-b9b3-11ea-81ac-d5943849cd53.png)
![Ring2](https://user-images.githubusercontent.com/39527815/85962551-99dd7980-b9b1-11ea-9be6-991d92da35f2.png)

A bridge connects any two corner cells

![EndGame - Bridge](https://user-images.githubusercontent.com/39527815/85962556-9f3ac400-b9b1-11ea-99c6-266ea09a0b4a.png)

A fork connects any three edges (corner cells are not considered as part of edges)

![EndGame - Fork](https://user-images.githubusercontent.com/39527815/85962767-ccd43d00-b9b2-11ea-809a-f73a6c582ef2.png)
