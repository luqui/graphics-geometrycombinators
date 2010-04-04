module Graphics.GeometryCombinators 
    ( R, R3, Color(..), modulate
    , Geometry
    , render
    , triangle
    , translate, rotate, scale
    , tint
    )
where

import qualified Graphics.Rendering.OpenGL.GL as GL

type R = GL.GLdouble
type R3 = (R,R,R)

data Color = Color !R !R !R !R

modulate :: Color -> Color -> Color
modulate (Color r g b a) (Color r' g' b' a') = Color (r*r') (g*g') (b*b') (a*a')

newtype Geometry = Geometry { renderG :: Color -> IO () }

render :: Geometry -> IO ()
render geom = renderG geom (Color 1 1 1 1)

triangle :: R3 -> R3 -> R3 -> Geometry
triangle p1 p2 p3 = Geometry $ \col -> do
    glColor col
    GL.renderPrimitive GL.Triangles $ do
        vertex p1
        vertex p2
        vertex p3
    where
    vertex = GL.vertex . glVertex

glColor :: Color -> IO ()
glColor (Color r g b a) = GL.color (GL.Color4 r g b a)

glVertex :: R3 -> GL.Vertex3 R
glVertex (x,y,z) = GL.Vertex3 x y z

glVector :: R3 -> GL.Vector3 R
glVector (x,y,z) = GL.Vector3 x y z

translate :: R3 -> Geometry -> Geometry
translate p geom = Geometry $ \col -> GL.preservingMatrix $ do
    GL.translate (glVector p)
    renderG geom col

rotate :: R3 -> R -> Geometry -> Geometry
rotate axis angle geom = Geometry $ \col -> GL.preservingMatrix $ do
    GL.rotate angle (glVector axis)
    renderG geom col

scale :: R -> R -> R -> Geometry -> Geometry
scale sx sy sz geom = Geometry $ \col -> GL.preservingMatrix $ do
    GL.scale sx sy sz
    renderG geom col

tint :: Color -> Geometry -> Geometry
tint col geom = Geometry $ \col' -> renderG geom (col' `modulate` col)
