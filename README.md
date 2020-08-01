# elm-obj-file

An Elm package to decode 3D models from the [OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file). Meshes are returned as [`TriangularMesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest) values, which makes them easy to render with [elm-3d-scene](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest) but can also be used with any other 3D graphics system. You could even take the geometric data and use it for 3D printing, physics simulations, [finite element analysis](https://en.wikipedia.org/wiki/Finite_element_method) or whatever other crazy thing you want to do =)

```elm
{-| Load a mesh from an HTTP request. -}
getMesh : Cmd Msg
getMesh =
    Http.get
        { url = "Pod.obj.txt"
        , expect =
            Obj.Decode.expectObj GotMesh
                Length.centimeters
                Obj.Decode.texturedFaces
        }
```

_Note the .txt extension: this is currently required to serve files from `elm reactor`._

## Blender Workflow

To export an OBJ file from Blender please refer to [the official manual](https://docs.blender.org/manual/en/2.83/addons/import_export/scene_obj.html?highlight=obj).

If you want to use the shadow generation functionality from `elm-3d-scene`, your mesh needs to be watertight. Blender has the 3D Print Toolbox add-on, that lets you detect non manifold edges and fix them by clicking the “Make Manifold” button.

## OBJ Format Support

- [x] different combinations of positions, normal vectors and UV (texture coordinates);
- [x] face elements `f`;
- [x] line elements `l`;
- [x] points elements `p`;
- [x] object names `o`;
- [x] group names `g`;
- [x] material names `usemtl`;
- [ ] smoothing groups `s`;
- [ ] free-form curves and surfaces and related data;
- [ ] miscellaneous display and rendering data attributes, e.g. `mtllib`.
