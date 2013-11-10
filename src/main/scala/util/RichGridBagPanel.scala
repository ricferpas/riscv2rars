package util

import swing._

trait RichGridBagPanel { this: GridBagPanel ⇒
  def at[A <: Component](x: Int, y: Int, w: Int, h: Int, wx: Float = 1, wy: Float = 1)(c: A) = {
    layout(c) = new Constraints {
      gridx = x
      gridy = y
      gridwidth = w
      gridheight = h
      fill = GridBagPanel.Fill.Both
      weightx = wx
      weighty = wy
      insets = new Insets(5, 5, 5, 5)
    }
    c
  }
}
