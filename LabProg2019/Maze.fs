(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx
open System
open System.Text
open Engine

[<Diagnostics.DebuggerDisplay("{ ToString ()}")>]

type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    sprites: sprite[]
    score:int
}
let exists (x : 'a option) =
    match x with
    | Some(x) -> true
    | None -> false
let getRandomVicino(x:int,y:int,maxX:int,maxY:int,matrice:cell[,] )=       
    let mutable i = 0
    let l =[|
        if((x-1)>=0 && (x-1)<maxX && y>=0 && y<maxY && (matrice.[x-1,y].visited=false) ) then yield false,(matrice.[x-1,y]);i<-i+1   //Sinistra
        if((x+1)>=0 && (x+1)<maxX && y>=0 && y<maxY && (matrice.[x+1,y].visited=false)) then yield false,(matrice.[x+1,y]);i<-i+1     //Destra
        if((x)>=0 && x<maxX && (y-1)>=0 && (y-1)<maxY && (matrice.[x,y-1].visited=false)) then yield false,(matrice.[x,y-1]);i<-i+1     //Sopra
        if((x)>=0 && x<maxX && (y+1)>=0 && (y+1)<maxY && (matrice.[x,y+1].visited=false)) then yield false,(matrice.[x,y+1]);i<-i+1     //Sotto
    |]
    let r = System.Random().Next(0, i)
    if i = 0 then 
        (true, (new cell()))
    else l.[r]
//La differenza con quello di prima è che mi prende le celle vicine con un percorso(ci sono i muri aperti)
let getPossiblePath(x:int,y:int,maxX:int,maxY:int,matrice:cell[,])=
    let mutable i = 0
    let l =[|
        if((x-1)>=0 && (x-1)<maxX && y>=0 && y<maxY && (matrice.[x-1,y].visited=false) && matrice.[x,y].topWall=false) then yield false,(matrice.[x-1,y]);i<-i+1   //sopra
        if((x+1)>=0 && (x+1)<maxX && y>=0 && y<maxY && (matrice.[x+1,y].visited=false) && matrice.[x,y].bottomWall=false ) then yield false,(matrice.[x+1,y]);i<-i+1     //sotto
        if((x)>=0 && x<maxX && (y-1)>=0 && (y-1)<maxY && (matrice.[x,y-1].visited=false) && matrice.[x,y].leftWall=false) then yield false,(matrice.[x,y-1]);i<-i+1     //sinistra
        if((x)>=0 && x<maxX && (y+1)>=0 && (y+1)<maxY && (matrice.[x,y+1].visited=false)&& matrice.[x,y].rightWall=false ) then yield false,(matrice.[x,y+1]);i<-i+1     //destra
    |]
    let r = System.Random().Next(0, i)
    if i = 0 then 
        (true, (new cell()))
    else l.[r]


let getPossiblePathNemico(x:int,y:int,maxX:int,maxY:int,matrice:cell[,])=
    let mutable i = 0
    let l =[|
        if((x-1)>=0 && (x-1)<maxX && y>=0 && y<maxY && matrice.[x,y].topWall=false) && matrice.[x-1,y].enemy=false then yield false,(matrice.[x-1,y]);i<-i+1   //Sinistra
        if((x+1)>=0 && (x+1)<maxX && y>=0 && y<maxY && matrice.[x,y].bottomWall=false ) && matrice.[x+1,y].enemy=false then yield false,(matrice.[x+1,y]);i<-i+1     //Destra
        if((x)>=0 && x<maxX && (y-1)>=0 && (y-1)<maxY && matrice.[x,y].leftWall=false) && matrice.[x,y-1].enemy=false then yield false,(matrice.[x,y-1]);i<-i+1     //Sopra
        if((x)>=0 && x<maxX && (y+1)>=0 && (y+1)<maxY && matrice.[x,y].rightWall=false )&& matrice.[x,y+1].enemy=false then yield false,(matrice.[x,y+1]);i<-i+1     //Sotto
    |]
    let r = System.Random().Next(0, i)
    if i = 0 then 
        (true, (new cell()))
    else l.[r]

let getBool((b,c):bool*cell)=b
let getCell((b,c):bool*cell)=c

let rec push(l:'a list, elemento:'a)=
    match l with
        |[]->[elemento]
        |[x] -> [x]@[elemento]
        |x::xs->x::push(xs,elemento)
        
let cancellaMuri(c1:cell,c2:cell)=
    let Deltax = c1.x-c2.x
    let Deltay = c1.y-c2.y
    if Deltax = 1 then
        c1.leftWall<-false
        c2.rightWall<-false
    if Deltax = -1 then
        c1.rightWall<-false
        c2.leftWall<-false
    if Deltay = -1 then
        c1.bottomWall<-false
        c2.topWall<-false
    if Deltay = 1 then
        c1.topWall<-false
        c2.bottomWall<-false
// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
let labirintoVisitato(struttura:cell[,],h:int,w:int)=
    let mutable k=0
    let b = [|
        for i in 0..h-1 do
            for j in 0..w-1 do
                if struttura.[i,j].visited=false then
                    k<-k+1
    |]
    if k>0 then true else false
type maze(w , h ,solve:bool) =
    member val Struttura:cell[,] = Array2D.create h w (new cell())
    

    member this.get(x:int,y:int):cell= 
        Array2D.get this.Struttura x y
    member this.getByCoordinates(y:int,x:int):cell= 
        Array2D.get this.Struttura x y
    member val monete:int = 0 with get,set
    member this.generate=
     //Inizializzo la matrice
         for i in 0..h-1 do 
             for j in 0..w-1 do
                 this.Struttura.[i,j]<-new cell()
                 this.Struttura.[i,j].x<-j
                 this.Struttura.[i,j].y<-i
         this.Struttura.[0,0].visited<-true
         this.Struttura.[0,0].topWall<-false
         let mutable i = 0
         let mutable j = 0
         let mutable current = getRandomVicino(i,j,h,w,this.Struttura)
         cancellaMuri(getCell(current),this.Struttura.[0,0])
         let mutable next = false,new cell()
         getCell(current).visited<-true

         //Generazione
         let mutable stack:(bool*cell) list = []
         stack<-(push(stack,current))
         while labirintoVisitato(this.Struttura,h,w)=true do
             if getBool(current)=false then
                j<-(getCell(current).x)
                i<-(getCell(current).y)
                Log.msg "Exploring node: (%d,%d)" j i
                next <- getRandomVicino(i,j,h,w,this.Struttura)
                if getBool(next)=false then
                    getCell(next).visited<-true
                    //apro i muri
                    cancellaMuri(getCell(next),getCell(current))
            
                    current<-next
                    stack<-(push(stack,current))
                else 
                    if stack.Length>0 then 
                        current<-stack.Head
                        stack<-stack.Tail
             else 
                 if stack.Length>0 then 
                     current<-stack.Head
                     stack<-stack.Tail
         //Creo un uscita: 
            //dato che deve essere su un bordo, stabilisco se i o j sono uguali a zero
         let r = System.Random().Next(0, 2)
         if r = 0 then
            i<-0
            j<-System.Random().Next(1, w)
         else
            j<-0
            i<-System.Random().Next(1, h)
        
         if i = 0 then this.Struttura.[i,j].topWall<-false
         if j = 0 then this.Struttura.[i,j].leftWall<-false
         if i = h-1 then this.Struttura.[i,j].rightWall<-false
         if j = w-1 then this.Struttura.[i,j].bottomWall<-false
         this.Struttura.[i,j].finishLine<-true
         //reset visited<-false
         let r = System.Random()
         
         //Eventuale reset del labirinto + coin + nemici
         for i in 0..h-1 do 
            for j in 0..w-1 do
            this.Struttura.[i,j].visited<- solve
            if r.Next(0,50)=1 then
                this.Struttura.[i,j].enemy<-true
            else if r.Next(0,10)=1 && i<>0 && j<>0 then
                    this.monete<-this.monete+1
                    this.Struttura.[i,j].coin<-true  
         this.Struttura.[0,0].visited<-true
    
            
     //La prima cella e' visitata sicuramente
let trovaNemici(maze:maze,h:int,w:int)=
    let mutable nemici: cell List=[]
    for i in 0..w-1 do 
        for j in 0..h-1 do
            if maze.Struttura.[i,j].enemy=true then
                let a = maze.Struttura.[i,j]
                nemici<-push(nemici,a)
    nemici
let MuoviNemici(maze:maze,h:int,w:int,st:state,engine:engine,grandezzaCella:int)=
    let nemici:cell list=trovaNemici(maze,h,w)
    for i in 0..nemici.Length-1 do
        Log.msg "Ho mosso uno sprite"
        let CellaInCuiMiSposto=getPossiblePathNemico(nemici.[i].y,nemici.[i].x,h,w,maze.Struttura)
        if getBool(CellaInCuiMiSposto)=false then
            getCell(CellaInCuiMiSposto).enemy<-true
            nemici.[i].enemy<-false

            //Sovrascrivo lo sprite vecchio
            let immagine = image.cella (grandezzaCella, grandezzaCella, Color.Blue,nemici.[i])
            let NumeroMappato=nemici.[i].x+nemici.[i].y*h
            st.sprites.[NumeroMappato].clear
            st.sprites.[NumeroMappato]<-new sprite (immagine, int st.sprites.[NumeroMappato].x, int st.sprites.[NumeroMappato].y, int st.sprites.[NumeroMappato].z+1)
            engine.register_sprite st.sprites.[NumeroMappato]
            // Sovrascrivo lo sprite nuovo
            let immagine = image.cella (grandezzaCella, grandezzaCella, Color.Blue,getCell(CellaInCuiMiSposto))
            let NumeroMappato=(getCell(CellaInCuiMiSposto).x)+(getCell(CellaInCuiMiSposto).y)*h
            st.sprites.[NumeroMappato].clear
            st.sprites.[NumeroMappato]<-new sprite (immagine, int st.sprites.[NumeroMappato].x, int st.sprites.[NumeroMappato].y, int st.sprites.[NumeroMappato].z+1)
            engine.register_sprite st.sprites.[NumeroMappato]
               

    

                     

        