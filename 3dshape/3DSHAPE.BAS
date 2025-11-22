DECLARE SUB DisplayHelp ()
OPTION BASE 0
OPTION EXPLICIT

DEFINT A-Z

CONST AXESEX = 0
CONST AXESEY = 1
CONST AXESEZ = 2
CONST DEFAULT_ZOOM# = 200
CONST COMMENT_MARKER$ = "#"
CONST FALSE = 0
CONST LINE_MARKER$ = "l"
CONST PI# = 3.14159265358979#
CONST MAXIMUM_ANGLE# = PI# * 2
CONST TRUE = -1
CONST VERTEX_MARKER$ = "v"

TYPE PointF
   X AS SINGLE
   Y AS SINGLE
END TYPE

TYPE LineStr
   Start AS LONG
   EndV AS LONG
END TYPE

TYPE Vertex3dstr
   X AS DOUBLE
   Y AS DOUBLE
   Z AS DOUBLE
END TYPE

DECLARE FUNCTION AdjustAngle (Angle AS DOUBLE, Increase AS INTEGER, Change AS DOUBLE) AS DOUBLE
DECLARE FUNCTION GetMaximumCoordinate (Vertices() AS Vertex3dstr) AS DOUBLE
DECLARE SUB DrawShape ()
DECLARE SUB InitializeDisplayParameters ()
DECLARE SUB LoadShape (FilePath AS STRING)
DECLARE SUB Main ()
DECLARE SUB Quit ()
DECLARE SUB Rotate (Vertex AS Vertex3dstr, Angle AS DOUBLE, Axis AS INTEGER)
DECLARE SUB Split (Text AS STRING, Items() AS STRING)

DIM SHARED AngleX AS DOUBLE
DIM SHARED AngleY AS DOUBLE
DIM SHARED AngleZ AS DOUBLE
DIM SHARED CameraZ AS LONG
DIM SHARED Lines() AS LineStr
DIM SHARED NewVertex AS Vertex3dstr
DIM SHARED Vertices() AS Vertex3dstr
DIM SHARED Zoom AS DOUBLE

   IF LTRIM$(RTRIM$(COMMAND$)) = "" THEN
      PRINT "Usage: 3DShape.exe SHAPE_FILE"
      SYSTEM
   ELSE
      LoadShape COMMAND$
      CALL Main
      Quit
   END IF


FUNCTION AdjustAngle (Angle AS DOUBLE, Increase AS INTEGER, Change AS DOUBLE) AS DOUBLE
   IF Increase THEN
      IF Angle + Change > MAXIMUM_ANGLE# THEN
         Angle = (Angle + Change) - MAXIMUM_ANGLE#
      ELSE
         Angle = Angle + Change
      END IF
   ELSE
      IF Angle - Change < 0 THEN
         Angle = MAXIMUM_ANGLE# + (Angle - Change)
      ELSE
         Angle = Angle - Change
      END IF
   END IF

   AdjustAngle = Angle
END FUNCTION

SUB DisplayHelp ()
   CLS
   COLOR 15
   LOCATE 3, 4
   PRINT "Help"
   PRINT
   PRINT "   Keys and functions:"
   PRINT "   Arrow keys and page up/down = Rotate x/y/z."
   PRINT "   R = Reset."
   PRINT "   +/- = Zoom in/out."
   PRINT
   DO
   LOOP WHILE INKEY$ = ""
END SUB

SUB DrawShape ()
DIM DisplayedShape() AS PointF
DIM Factor AS DOUBLE
DIM Index AS INTEGER
DIM RotatedVertex AS Vertex3dstr
   
   REDIM DisplayedShape(0 TO 0) AS PointF
   FOR Index = LBOUND(Vertices) TO UBOUND(Vertices)
      RotatedVertex.X = Vertices(Index).X
      RotatedVertex.Y = Vertices(Index).Y
      RotatedVertex.Z = Vertices(Index).Z

      Rotate RotatedVertex, AngleX, AXESEX
      RotatedVertex = NewVertex

      Rotate RotatedVertex, AngleY, AXESEY
      RotatedVertex = NewVertex

      Rotate RotatedVertex, AngleZ, AXESEZ
      RotatedVertex = NewVertex

      Factor = Zoom / (RotatedVertex.Z + CameraZ)
      DisplayedShape(UBOUND(DisplayedShape)).X = CSNG((RotatedVertex.X * Factor) + 320)
      DisplayedShape(UBOUND(DisplayedShape)).Y = CSNG((-RotatedVertex.Y * Factor) + 200)
      IF Index < UBOUND(Vertices) THEN REDIM PRESERVE DisplayedShape(LBOUND(DisplayedShape) TO UBOUND(DisplayedShape) + 1) AS PointF
   NEXT Index

   CLS
   FOR Index = LBOUND(Lines) TO UBOUND(Lines)
      LINE (DisplayedShape(Lines(Index).Start).X, DisplayedShape(Lines(Index).Start).Y)-(DisplayedShape(Lines(Index).EndV).X, DisplayedShape(Lines(Index).EndV).Y), 15
   NEXT Index
END SUB

FUNCTION GetMaximumCoordinate (Vertices() AS Vertex3dstr) AS DOUBLE
DIM Index AS INTEGER
DIM Maximum AS DOUBLE
DIM MaximumCoordinate AS DOUBLE

   FOR Index = LBOUND(Vertices) TO UBOUND(Vertices)
      IF ABS(Vertices(Index).X) >= ABS(Vertices(Index).Y) AND ABS(Vertices(Index).X) >= (Vertices(Index).Z) THEN
         Maximum = ABS(Vertices(Index).X)
      ELSEIF ABS(Vertices(Index).Y) >= ABS(Vertices(Index).X) AND ABS(Vertices(Index).Y) >= ABS(Vertices(Index).Z) THEN
         Maximum = ABS(Vertices(Index).Y)
      ELSEIF ABS(Vertices(Index).Z) >= ABS(Vertices(Index).X) AND ABS(Vertices(Index).Z) >= ABS(Vertices(Index).Y) THEN
         Maximum = ABS(Vertices(Index).Z)
      END IF

      IF Maximum >= MaximumCoordinate THEN MaximumCoordinate = Maximum
   NEXT Index

   GetMaximumCoordinate = MaximumCoordinate
END FUNCTION

SUB InitializeDisplayParameters ()
   AngleX = 0
   AngleY = 0
   AngleZ = 0
   Zoom = DEFAULT_ZOOM#
END SUB

SUB LoadShape (FilePath AS STRING)
DIM FileH AS INTEGER
DIM Items() AS STRING
DIM LineV AS STRING

   REDIM Lines(0 TO 0) AS LineStr
   REDIM Vertices(0 TO 0) AS Vertex3dstr
   
   FileH = FREEFILE
   OPEN FilePath FOR INPUT LOCK READ WRITE AS FileH
      DO UNTIL EOF(FileH)
         LINE INPUT #FileH, LineV
         LineV = LTRIM$(RTRIM$(LineV))
   
         IF NOT LineV = "" THEN
            IF NOT LEFT$(LineV, LEN(COMMENT_MARKER$)) = COMMENT_MARKER$ THEN
               Split LineV, Items()
              
               SELECT CASE LCASE$(LTRIM$(RTRIM$(Items(0))))
                  CASE LINE_MARKER$
                     IF UBOUND(Items) - LBOUND(Items) >= 2 THEN
                        Lines(UBOUND(Lines)).Start = CDBL(VAL(Items(1)))
                        Lines(UBOUND(Lines)).EndV = CDBL(VAL(Items(2)))
                        IF NOT EOF(FileH) THEN
                           REDIM PRESERVE Lines(LBOUND(Lines) TO UBOUND(Lines) + 1) AS LineStr
                        END IF
                     END IF
                  CASE VERTEX_MARKER$
                     IF UBOUND(Items) - LBOUND(Items) >= 3 THEN
                        Vertices(UBOUND(Vertices)).X = CDBL(VAL(Items(1)))
                        Vertices(UBOUND(Vertices)).Y = CDBL(VAL(Items(2)))
                        Vertices(UBOUND(Vertices)).Z = CDBL(VAL(Items(3)))
                        IF NOT EOF(FileH) THEN
                           REDIM PRESERVE Vertices(LBOUND(Vertices) TO UBOUND(Vertices) + 1) AS Vertex3dstr
                        END IF
                     END IF
               END SELECT
            END IF
         END IF
      LOOP
   CLOSE FileH
   
   CameraZ = CINT(GetMaximumCoordinate(Vertices()) * 2)

   InitializeDisplayParameters
END SUB

SUB Main ()
DIM KeyStroke AS STRING

   SCREEN 12
   CLS
   
   InitializeDisplayParameters
   DO UNTIL KeyStroke = CHR$(27)
      DrawShape
      COLOR 15
      LOCATE 1, 1
      PRINT "3D Shapes v1.00 - by: Peter Swinkels, ***2025***"
      PRINT "F1 = Help   Esc = Quit"
      DO
         KeyStroke = INKEY$
      LOOP WHILE KeyStroke = ""
      SELECT CASE KeyStroke
         CASE CHR$(0) + "H"
            AngleX = AdjustAngle(AngleX, FALSE, .05)
         CASE CHR$(0) + "I"
            AngleZ = AdjustAngle(AngleZ, FALSE, .05)
         CASE CHR$(0) + "K"
            AngleY = AdjustAngle(AngleY, FALSE, .05)
         CASE CHR$(0) + "M"
            AngleY = AdjustAngle(AngleY, TRUE, .05)
         CASE CHR$(0) + "P"
            AngleX = AdjustAngle(AngleX, TRUE, .05)
         CASE CHR$(0) + "Q"
            AngleZ = AdjustAngle(AngleZ, TRUE, .05)
         CASE CHR$(0) + ";"
            DisplayHelp
         CASE "R", "r"
            InitializeDisplayParameters
         CASE "-"
            Zoom = Zoom - 10
         CASE "+"
            Zoom = Zoom + 10
      END SELECT
   LOOP
END SUB

SUB Quit ()
   RESET
   SCREEN 0
   WIDTH 80, 25
   COLOR 7, 0
   CLS
   SYSTEM
END SUB

SUB Rotate (Vertex AS Vertex3dstr, Angle AS DOUBLE, Axis AS INTEGER)
   SELECT CASE Axis
      CASE AXESEX
         NewVertex.X = Vertex.X
         NewVertex.Y = (Vertex.Y * COS(Angle)) - Vertex.Z * SIN(Angle)
         NewVertex.Z = (Vertex.Y * SIN(Angle)) + Vertex.Z * COS(Angle)
      CASE AXESEY
         NewVertex.X = (Vertex.X * COS(Angle)) + Vertex.Z * SIN(Angle)
         NewVertex.Y = Vertex.Y
         NewVertex.Z = (-Vertex.X * SIN(Angle)) + (Vertex.Z * COS(Angle))
      CASE AXESEZ
         NewVertex.X = (Vertex.X * COS(Angle)) - (Vertex.Y * SIN(Angle))
         NewVertex.Y = (Vertex.X * SIN(Angle)) + (Vertex.Y * COS(Angle))
         NewVertex.Z = Vertex.Z
   END SELECT
END SUB

SUB Split (Text AS STRING, Items() AS STRING)
DIM NextPosition AS INTEGER
REDIM Items(0 TO 0) AS STRING

   DO
      NextPosition = INSTR(Text, " ")
      IF NextPosition = 0 THEN NextPosition = LEN(Text)
      Items(UBOUND(Items)) = LTRIM$(RTRIM$(MID$(Text, 1, NextPosition)))
      Text = LTRIM$(RTRIM$(MID$(Text, NextPosition + 1)))
      IF Text = "" THEN EXIT DO
      REDIM PRESERVE Items(LBOUND(Items) TO UBOUND(Items) + 1) AS STRING
   LOOP
END SUB

