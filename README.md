# elm-obj-file

An Elm package to encode and decode 3D geometry in the [OBJ file format](https://en.wikipedia.org/wiki/Wavefront_.obj_file). Meshes are returned as [`TriangularMesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest) values, which makes them easy to render with [elm-3d-scene](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest) but can also be used with any other 3D graphics system. You could even take the geometric data and use it for 3D printing, [physics simulations](https://package.elm-lang.org/packages/w0rm/elm-physics/latest/), [finite element analysis](https://en.wikipedia.org/wiki/Finite_element_method) or whatever other crazy thing you want to do =)

![The “Pod” model by Kolja Wilcke](https://unsoundscapes.com/elm-obj-file/1.2.1/examples/pod.png)

_The “Pod” model by [@01k](https://mobile.twitter.com/01k) rendered with `elm-3d-scene`. [See it live here](https://unsoundscapes.com/elm-obj-file/1.2.1/examples/pod/)._

Make sure to check [the viewer example](https://unsoundscapes.com/elm-obj-file/1.2.1/examples/viewer/) that lets you preview OBJ files.

The examples source code [can be found here](https://github.com/w0rm/elm-obj-file/tree/5ac5b9eec44c7c52db5d8f1a4415633b02150c7d/examples).

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

To export an OBJ file from Blender choose `File - Export - Wavefront (.obj)`. We recommend the following settings:

- **Include:** only check “Objects as OBJ Objects”;
- **Transform:** use scale `1.00`, “Y Forward” and “Z Up” to match the Blender coordinate system;
- **Geometry:** only check “Apply Modifiers”, check “Write Normals” for `Obj.Decode.faces` and `Obj.Decode.texturedFaces`, “Include UVs” for `Obj.Decode.texturedTriangles` and `Obj.Decode.texturedFaces`, optionally check “Write Materials” if you want to decode material names.

Blender collections are not preserved in OBJ groups. To decode individual meshes from the same file, you should rely on the `object` filter. The object name, that Blender produces, is a concatenation of the corresponding object and geometry. For example, the “Pod Body” object that contains “Mesh.001” can be decoded with `Obj.Decode.object "Pod_Body_Mesh.001"`.

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
