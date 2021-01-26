; thanks to SSWIFT for the nuts and bolts of this code 

AppTitle "weapon Movement Demo "
Graphics3D 1024,768,32
HidePointer 



;-------------------------------------------------------------------------------------\ 
; GLOBALS 
;------------------------------------------------------------------------------------- 

; world vars 
Global cam,lite,weapon,world,weapon_type,world_type 

Const GRAVITY# = .00981 * 2
Const AIR_FRICTION_CONSTANT# = 0.01 
Const GROUND_FRICTION_CONSTANT# = .0030 
Global bullet_life = 2000

Global weaponstyle = 1

Global Target_X# = 0.25
Global Target_Y# = 0.5
Global Target_Z# = 0.125


Global goals, weaponcount

Global jumping,player,vector_piv, box
Dim weapontex(5)

Global midw = GraphicsWidth()/2
Global midh = GraphicsHeight()/2 
Global speed#,lateral_speed#,cam_mx#,cam_my#,pyvel# 
Global shot_timer 

Type weapon 
   Field entity
   Field life
   Field recharge
   Field rotate
   Field brush
   Field radius#
   Field Mass#
   Field size#
   Field vx#,vy#,vz# 
   Field oldx#,oldy#,oldz#
   Field newx#,newy#,newz# 
End Type 

SeedRnd MilliSecs 
initialize_world() 

; ------------------------------------------------------------------------------- 
Repeat
   shot_timer = shot_timer - 1 : If shot_timer < 1 Then shot_timer = 0 

   target_handle()
   update_weapon()
   fps_camera() 
   UpdateWorld 
   RenderWorld

   Color 255,255,255
   Text 0,0,"SCORE: " + goals
   Text 0,15,"Weapons on screen = " + weaponcount

   Select weaponstyle
   Case 1 txt$ = "AmigaBall"
   Case 2 txt$ = "PoolBall"
   Case 3 txt$ = "HandBall"
   Case 4 txt$ = "BeachBall"
   Case 5 txt$ = "RubberDisc"
   End Select
   Text 0,30, "Current weapon: " + txt$

   Flip
Until KeyDown(1) = 1

ClearWorld 
End 

;===================================================================================== 
; FUNCTIONS 
;===================================================================================== 
Function target_handle()
   If EntityX# (box) > 80 Or EntityX# (box) < 40 Then Target_X# = -Target_X#
   If EntityY# (box) > 40 Or EntityY# (box) < -40 Then Target_Y# = -Target_Y#
   If EntityZ# (box) > 40 Or EntityZ# (box) < -40 Then Target_Z# = -Target_Z#

   MoveEntity box,Target_X#,Target_Y#,Target_Z#
   RotateMesh box,0,1,0 

End Function


Function create_checker_tex(red1,green1,blue1,red2,green2,blue2,scale_u#,scale_v#) 
   texture_handle = CreateTexture(32,32,256) 
   SetBuffer TextureBuffer(texture_handle) 
   Color red1,green1,blue1 
   Rect 0,0,32,32
   Color red2,green2,blue2 
   Rect 0,0,16,16,1 
   Rect 16,16,16,16,1 
   ScaleTexture texture_handle,scale_u#,scale_v# 
   SetBuffer BackBuffer() 
   Return texture_handle 

End Function 


Function create_pox_tex(red1,green1,blue1,red2,green2,blue2,scale_u#,scale_v#) 
   texture_handle = CreateTexture(32,32,256) 
   SetBuffer TextureBuffer(texture_handle) 
   Color red1,green1,blue1 
   Rect 0,0,32,32
   Color red2,green2,blue2 
   Oval 0,0,16,16,1 
   Oval 16,16,16,16,1 
   ScaleTexture texture_handle,scale_u#,scale_v# 
   SetBuffer BackBuffer() 
   Return texture_handle 

End Function 


Function create_stripe_tex(direction,scale_u#,scale_v#)
   If direction = 1 Then h = 1: v = 0
   If direction = 2 Then h = 0: v = 1

   texture_handle = CreateTexture(32,32,256) 
   SetBuffer TextureBuffer(texture_handle)
   Color 255,255,0 
   Rect 0,0,32,32
   Color 0,255,0 
   Rect 8 * h, 8 * v, 32 * v + 8 * h, 8 * v + 32 * h
   Color 255,0,0 
   Rect 16 * h, 16 * v, 32 * v + 8 * h, 8 * v + 32 * h
   Color 0,0,255 
   Rect 24 * h, 24 * v, 32 * v + 8 * h, 8 * v + 32 * h
   ScaleTexture texture_handle,scale_u#,scale_v# 
   SetBuffer BackBuffer() 
   Return texture_handle 

End Function 


;===================================================================================== 
Function update_weapon() 
   weaponcount = 0

   For b.weapon = Each weapon
      weaponcount = weaponcount + 1
      ; goal counting, change goal color
      Entity_Hit = EntityCollided(b\entity, world_type) 
      If Entity_Hit Then
         If CollisionEntity(b\entity,1) = box 
            goals = goals + 1
            EntityColor box, Rnd(0,127) + 128, Rnd(0,127) + 128, Rnd(0,127) + 128
         EndIf
      EndIf

      b.weapon = apply_physics.weapon (b.weapon, Entity_Hit)

      Entity_Hit = EntityCollided(b\entity, weapon_type) 
      If Entity_Hit Then
         b.weapon = collided_with.weapon(Entity_Hit, b.weapon)
      EndIf
   Next

   For b.weapon = Each weapon
      b\OldX# = b\NewX#
      b\OldY# = b\NewY#
      b\OldZ# = b\NewZ#

      TranslateEntity b\entity, b\Vx#, b\Vy#, b\Vz#, True 

      b\NewX# = EntityX#(b\entity, True) 
      b\NewY# = EntityY#(b\entity, True) 
      b\NewZ# = EntityZ#(b\entity, True)
         XAngleAdjust# = ((b\NewX# - b\OldX#) / b\radius#) * (90.0 / Pi)
         YAngleAdjust# = ((b\NewY# - b\OldY#) / b\radius#) * (90.0 / Pi)
         ZAngleAdjust# = ((b\NewZ# - b\OldZ#) / b\radius#) * (90.0 / Pi)

      If b\rotate = True Then
         TurnEntity b\entity, ZAngleAdjust#, 0, -XAngleAdjust#, True
      Else
         TurnEntity b\entity, 0, ZAngleAdjust# - XAngleAdjust#, 0, True
      EndIf

      b\life = b\life - 1 
      If b\life = 0 
         FreeEntity b\entity
         FreeBrush b\brush
         Delete b.weapon
      ElseIf b\life < 50 Then
         EntityAlpha b\entity,b\life * 0.02
      EndIf 
   Next
End Function 


Function collided_with.weapon(Entity_Hit, x.weapon)
   For b.weapon = Each weapon
      If b\entity = Entity_Hit Then
         xx# = x\Vx#
         xy# = x\Vy#
         xz# = x\Vz#

         Velocity# = Sqr(b\Vx#^2 + b\Vy#^2 + b\Vz#^2)
         VdotN# = GROUND_FRICTION_CONSTANT# * Velocity# / x\Mass#
         x\Vx# = (b\Vx# - VdotN#)
         x\Vy# = (b\Vy# - VdotN#)
         x\Vz# = (b\Vz# - VdotN#)

         Velocity# = Sqr(xx#^2 + xy#^2 + xz#^2)
         VdotN# = GROUND_FRICTION_CONSTANT# * Velocity# / b\Mass#
         b\Vx# = (xx# - VdotN#)
         b\Vy# = (xy# - VdotN#)
         b\Vz# = (xz# - VdotN#)

         Return x.weapon
      EndIf
   Next
End Function


Function apply_physics.weapon(x.weapon, Entity_Hit)
      Local Nx#, Ny#, Nz#, NFx#, NFy#, NFz#, VdotN#

      Velocity# = Sqr(x\Vx#^2 + x\Vy#^2 + x\Vz#^2)
      If Velocity# > 0 ; Calculate the direction vector. The direction vector has a length of 1. 

         Direction_X# = x\Vx# / Velocity#
         Direction_Y# = x\Vy# / Velocity#
         Direction_Z# = x\Vz# / Velocity#

         ; Compute air friction. ; Air friction is dependent on the speed of the entity, and will prevent it from accelerting forever. 
         Velocity# = Velocity# - (AIR_FRICTION_CONSTANT# * Velocity# * x\size# / x\Mass#)

         If (Velocity# < 0) Then Velocity# = 0

         ; Convert the entity's velocity and direction back into a motion vector.
         x\Vx# = Direction_X# * Velocity#
         x\Vy# = Direction_Y# * Velocity#
         x\Vz# = Direction_Z# * Velocity#

         ; If the entity collided with the level, apply ground friction. 
         If Entity_Hit > 0 ; Compute ground friction. Ground friction is not dependent on the speed of the entity. 
            Velocity# = Velocity# - (GROUND_FRICTION_CONSTANT# * Velocity# * x\size# / x\Mass#)
         EndIf 

         ; If the entity collided with the level, make it bounce. 
         If Entity_Hit > 0 Then
            ; Calculate bounce: 
            ; Get the normal of the surface which the entity collided with. 
            Nx# = CollisionNX(x\entity, 1)
            Ny# = CollisionNY(x\entity, 1)
            Nz# = CollisionNZ(x\entity, 1)
            ; Compute the dot product of the entity's motion vector and the normal of the surface collided with. 
            VdotN# = (x\Vx# * Nx# + x\Vy# * Ny# + x\Vz# * Nz#)

            ; Calculate the normal force. 
            NFx# = -2.0 * Nx# * VdotN#
            NFy# = -2.0 * Ny# * VdotN#
            NFz# = -2.0 * Nz# * VdotN#

            x\Vx# = x\Vx# + NFx#
            x\Vy# = x\Vy# + NFy#
            x\Vz# = x\Vz# + NFz#

         EndIf 
         ; Apply gravity: 
         x\Vy# = x\Vy# - (GRAVITY# * x\Mass#)

      EndIf 

      Return x.weapon
End Function


;===================================================================================== 
Function initialize_world() 
   cam = CreateCamera() 
   player = CreatePivot() 
   EntityRadius cam,3
   EntityRadius player,3
;   MoveEntity player,0,0,0 
   vector_piv = CreatePivot() 

   AmbientLight(50,50,50) 
   lite=CreateLight(3) 
   PositionEntity lite,30,-50,-50

   weapontex(1) = create_checker_tex(255,255,255,255,000,000,.25,.25)
   weapontex(2) = create_pox_tex    (000,255,255,000,000,255,.25,.25)
   weapontex(3) = create_stripe_tex (2,.25,.25)
   weapontex(4) = create_stripe_tex (1,.5,.5)
   weapontex(5) = create_checker_tex(000,255,000,255,000,255,1,1)

   world=CreateCube()

   BOX=CreateCube()
   EntityAlpha BOX,.60
   PositionEntity box,80,25,0 
   ScaleEntity box,2,6,2
   ScaleEntity world,120,120,120

   EntityTexture world,create_pox_tex (255,255,255,0,0,255,.125,.125) 

   FlipMesh world 

   ; SETUP COLLISIONS 
   weapon_type=1
   world_type=2 

   EntityType world,world_type 
   EntityType box,world_type 
   EntityColor box,255,0,0 

   EntityType player,weapon_type 
   EntityType cam,weapon_type

   Collisions weapon_type,world_type,2,2
   Collisions weapon_type,weapon_type,2,2


End Function 

;===================================================================================== 
Function fps_camera() 
   If KeyDown(17) Or KeyDown(200) Then speed# = speed# +.05
   If KeyDown(30) Or KeyDown(203) Then lateral_speed# = lateral_speed# - .05
   If KeyDown(31) Or KeyDown(208) Then speed# = speed# -.05
   If KeyDown(32) Or KeyDown(205) Then lateral_speed# = lateral_speed# + .05
   If KeyDown(57) Then VY#=VY#+.1 

   If KeyHit(02) Then weaponstyle = 1
   If KeyHit(03) Then weaponstyle = 2
   If KeyHit(04) Then weaponstyle = 3
   If KeyHit(05) Then weaponstyle = 4
   If KeyHit(06) Then weaponstyle = 5

   lateral_speed# = lateral_speed# * .97 
   speed# = speed# * .95 

   PositionEntity cam,EntityX(player),EntityY(player)+1,EntityZ(player) 

   ; CAMERA MOVEMENTS 
   cam_MY# = curvevalue#(MouseYSpeed(),cam_MY#,4 ) 
   cam_MX# = curvevalue#(MouseXSpeed(),cam_MX#,4 ) 
   TurnEntity cam,    cam_MY#, 0,       0 
   TurnEntity player, 0,       -cam_mx, 0 
   RotateEntity cam,EntityPitch(cam),EntityYaw(player),0 

   MoveMouse midw,midh

   If on_platform = 0 Then pyvel# = pyvel# - gravity# 

   MoveEntity player,lateral_speed#,pyvel#,speed# 

   If MouseDown(1) = 1 Then fire_cannon() 

   FlushMouse 

End Function 


;===================================================================================== 
Function curvevalue#(newvalue#,oldvalue#,increments# ) 
   If increments > 1 Then oldvalue#=oldvalue#-(oldvalue#-newvalue#)/increments 
   If increments <= 1 Then oldvalue=newvalue 
   Return oldvalue# 
End Function 


;===================================================================================== 
Function fire_cannon() 
   If shot_timer=0 

      b.weapon= New weapon


      If weaponstyle = 1 Then 
         b\radius# = 0.5
         b\size# = b\radius# * 2
         b\entity = CreateSphere(4 + b\size# * 2) 
         b\Mass# = 2.5
         b\recharge = 20
         b\rotate = True
         EntityType b\entity, weapon_type
         ScaleEntity  b\entity, b\size#, b\size#, b\size#
         EntityRadius b\entity, b\radius# * 2
         b\brush=CreateBrush()
         BrushTexture b\brush,weapontex(1)
         BrushColor b\brush,255,255,255 
      ElseIf weaponstyle = 2 Then
         b\radius# = 1
         b\size# = b\radius# * 2
         b\entity = CreateSphere(4 + b\size# * 2) 
         b\Mass# = 1
         b\recharge = 35
         b\rotate = True
         EntityType b\entity, weapon_type
         ScaleEntity  b\entity, b\size#, b\size#, b\size#
         EntityRadius b\entity, b\radius# * 2
         b\brush=CreateBrush()
         BrushTexture b\brush,weapontex(2)
         BrushColor b\brush,255,255,255 
      ElseIf weaponstyle = 3 Then
         b\radius# = 0.25
         b\size# = b\radius# * 2
         b\entity = CreateSphere(4 + b\size# * 2) 
         b\Mass# = 0.5
         b\recharge = 15
         b\rotate = True
         EntityType b\entity, weapon_type
         ScaleEntity  b\entity, b\size#, b\size#, b\size#
         EntityRadius b\entity, b\radius# * 2
         b\brush=CreateBrush()
         BrushTexture b\brush,weapontex(3)
         BrushColor b\brush,255,255,255 
      ElseIf weaponstyle = 4 Then
         b\radius# = 2.25
         b\size# = b\radius# * 2
         b\entity = CreateSphere(4 + b\size# * 2) 
         b\Mass# = 2.5
         b\recharge = 50
         b\rotate = True
         EntityType b\entity, weapon_type
         ScaleEntity  b\entity, b\size#, b\size#, b\size#
         EntityRadius b\entity, b\radius# * 2
         b\brush=CreateBrush()
         BrushTexture b\brush,weapontex(4)
         BrushColor b\brush,255,255,255
      ElseIf weaponstyle = 5 Then
         b\radius# = 0.5
         b\size# = b\radius# * 2
         b\entity = CreateCylinder(8 + b\size# * 2) 
         b\Mass# = 2
         b\recharge = 20
         b\rotate = False
         EntityType b\entity, weapon_type
         ScaleEntity  b\entity, b\size#, b\size#/5, b\size#
         EntityRadius b\entity, b\radius# * 2
         b\brush=CreateBrush()
         BrushTexture b\brush,weapontex(5)
         BrushColor b\brush,255,255,255
      EndIf

      PaintEntity b\entity,b\brush 

      PositionEntity b\entity, EntityX(cam), EntityY(cam)+b\radius#, EntityZ(cam) 
      PositionEntity vector_piv, EntityX(cam), EntityY(cam)+b\radius#, EntityZ(cam) 
      RotateEntity vector_piv, EntityPitch(cam), EntityYaw(cam), EntityRoll(cam) 
      MoveEntity vector_piv,0,0,5

      vectx# = EntityX(vector_piv) - EntityX(cam) 
      vecty# = EntityY(vector_piv) - EntityY(cam) 
      vectz# = EntityZ(vector_piv) - EntityZ(cam) 
      TFormVector vectx#,vecty#,vectz#,vector_piv,cam 

      b\vx = TFormedX()
      b\vz = TFormedZ()
      b\vy = TFormedY()

      b\life = bullet_life 
      shot_timer = b\recharge
   EndIf 

End Function 