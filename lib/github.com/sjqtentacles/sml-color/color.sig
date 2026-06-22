(* color.sig

   Pure color-space types and conversions. All channel values are reals in
   [0, 1] unless stated otherwise; hue is in degrees [0, 360). Functions are
   total and deterministic, byte-identical across MLton and Poly/ML. *)

signature COLOR =
sig
  (* Channels in [0,1]. *)
  type rgb  = { r : real, g : real, b : real }
  type rgba = { r : real, g : real, b : real, a : real }
  (* h in [0,360), s,v,l in [0,1]. *)
  type hsv  = { h : real, s : real, v : real }
  type hsl  = { h : real, s : real, l : real }

  (* CIE L*a*b* (D65). L* in [0,100]; a*,b* roughly [-128,127] but unbounded. *)
  type lab  = { l : real, a : real, b : real }
  (* Cylindrical Lab: L* as above, chroma c >= 0, hue h in degrees [0,360). *)
  type lch  = { l : real, c : real, h : real }

  (* Clamp every channel into its valid range (hue is wrapped mod 360). *)
  val clampRgb  : rgb  -> rgb
  val clampRgba : rgba -> rgba

  val rgbToRgba : real -> rgb -> rgba          (* supply alpha *)
  val rgbaToRgb : rgba -> rgb                  (* drop alpha *)

  (* HSV / HSL conversions. Achromatic inputs (s = 0) yield hue 0. *)
  val rgbToHsv : rgb -> hsv
  val hsvToRgb : hsv -> rgb
  val rgbToHsl : rgb -> hsl
  val hslToRgb : hsl -> rgb

  (* sRGB <-> linear transfer functions (per-channel), endpoints exact. *)
  val srgbToLinear : real -> real
  val linearToSrgb : real -> real
  val rgbToLinear  : rgb -> rgb
  val rgbToSrgb    : rgb -> rgb

  (* sRGB <-> CIE L*a*b*, via linearization, the sRGB/D65 XYZ matrix and the
     D65 white point (Xn,Yn,Zn) = (0.95047, 1.0, 1.08883). `toLab` does not
     clamp its input; `fromLab` clamps the recovered sRGB channels to [0,1]. *)
  val toLab   : rgb -> lab
  val fromLab : lab -> rgb

  (* Lab <-> LCh(ab): the polar form of a*,b*. Achromatic colors get hue 0. *)
  val labToLch : lab -> lch
  val lchToLab : lch -> lab
  val toLch    : rgb -> lch
  val fromLch  : lch -> rgb

  (* Perceptual color difference in Lab.
       deltaE76  -- CIE76, the plain Euclidean distance in L*a*b*.
       deltaE2000 -- CIEDE2000.
       deltaE    -- convenience: CIE76 between two sRGB colors. *)
  val deltaE76   : lab * lab -> real
  val deltaE2000 : lab * lab -> real
  val deltaE     : rgb * rgb -> real

  val approxLab : real -> lab * lab -> bool

  (* 32-bit packing, channel order 0xRRGGBBAA (R is the most significant byte).
     pack clamps to [0,1] and rounds to nearest. *)
  val pack   : rgba -> Word32.word
  val unpack : Word32.word -> rgba

  (* Hex strings. Parses "#rgb", "#rgba", "#rrggbb", "#rrggbbaa" (leading '#'
     optional, case-insensitive); returns NONE on malformed input. *)
  val fromHex : string -> rgba option
  val toHex   : rgba -> string                 (* canonical lowercase "#rrggbbaa" *)
  val toHexRgb : rgb -> string                 (* "#rrggbb" *)

  (* Interpolation. lerp is unclamped; mix = lerp with t clamped to [0,1]. *)
  val lerp : rgba * rgba * real -> rgba
  val mix  : rgba * rgba * real -> rgba

  val approx : real -> rgba * rgba -> bool
  val approxRgb : real -> rgb * rgb -> bool
end
