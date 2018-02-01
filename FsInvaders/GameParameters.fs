module GameParameters

let ScreenX = 400

let ScreenY = 600

let FormX = ScreenX + 100

let FormY = ScreenY + 80

let NumberOfInvaderColumns = 8

let NumberOfInvaderRows = 5

let InvAreaX = ScreenX / 100 * 80

let ColumnSize = InvAreaX / NumberOfInvaderColumns

let InvaderSize = ColumnSize * 5 / 8


let FrameInterval = 1000/60

let StartSpeed = 1000

let MaxSpeed = FrameInterval

let InvaderMove = ColumnSize / 2

let PlayerSpeed = 6

let PlayerWidth = 40

let PlayerHeight = 20


let hudHeight = 32

let MaxHighScores = 10

