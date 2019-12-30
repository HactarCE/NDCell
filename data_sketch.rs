state: struct State {
    grid_view: enum GridView {
        // Generalize over P
        View2D(struct GridView2D {
            automaton: enum ProjectedAutomaton<Dim2D> {
                // Generalize over D
                From1D(NdProjectedAutomaton<Dim1D, Dim2D>)
                From2D(NdProjectedAutomaton<Dim2D, Dim2D>)
                From3D(NdProjectedAutomaton<Dim3D, Dim2D> {
                    automaton: struct NdAutomaton<Dim3D> {
                        tree: NdTree<Dim3D>
                        sim: NdTree<Dim3D>
                        generations: usize
                    }
                    projection: struct NdProjection<Dim3D, Dim2D>
                })
                ...
            }
            viewport: Viewport2D
        })
        View3D(struct GridView3D {
            ...
        })
    }
}
