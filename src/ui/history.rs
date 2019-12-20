pub struct HistoryStack{
    undo: Vec<HistoryEntry>
    redo: Vec<HistoryEntry>
}

// #[enum_dispatch(NdHistoryEntryTrait)]
// pub enum HistoryEntry<D> {
//     Tree1D(NdHistoryEntry<Dim1>),
//     Tree2D(NdHistoryEntry<Dim2>),
//     Tree3D(NdHistoryEntry<Dim3>),
//     Tree4D(NdHistoryEntry<Dim4>),
//     Tree5D(NdHistoryEntry<Dim5>),
//     Tree6D(NdHistoryEntry<Dim6>),
// }

pub struct HistoryEntry {
    grid_view: GridView
}

impl NdHistoryEntry{
    fn restore(self, current: GridView) -> GridView {
        let ret = self;
        match &mut ret {
            GridView::View2D{automaton, viewport} => {
                // Preserve viewport if possible
                if let GridView::View2D{viewport: current_viewport, ..} = current {
                    *viewport = current_viewport;
                }
            }
            GridView::View3D{..} => {
                unimplemented!()
            }
        }
    }
}

impl From<GridView> for HistoryEntry {
    fn from(grid_view: GridView) -> Self {
        Self {grid_view}
    }
}
