module Form1

open System.Windows.Forms

type Form1() =
    class
        inherit Form()

        override _this.OnPreviewKeyDown(e: PreviewKeyDownEventArgs) =
            base.OnPreviewKeyDown(e)
            e.IsInputKey <- true

    end