(* color.sml

   Implementation of COLOR. Channels are reals in [0,1]; hue in degrees. *)

structure Color :> COLOR =
struct
  type rgb  = { r : real, g : real, b : real }
  type rgba = { r : real, g : real, b : real, a : real }
  type hsv  = { h : real, s : real, v : real }
  type hsl  = { h : real, s : real, l : real }
  type lab  = { l : real, a : real, b : real }
  type lch  = { l : real, c : real, h : real }

  fun clamp01 (x : real) = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x
  fun wrapHue h =
    let val m = Real.rem (h, 360.0)
    in if m < 0.0 then m + 360.0 else m end

  fun clampRgb ({r,g,b} : rgb) : rgb =
    { r = clamp01 r, g = clamp01 g, b = clamp01 b }
  fun clampRgba ({r,g,b,a} : rgba) : rgba =
    { r = clamp01 r, g = clamp01 g, b = clamp01 b, a = clamp01 a }

  fun rgbToRgba a ({r,g,b} : rgb) : rgba = { r = r, g = g, b = b, a = a }
  fun rgbaToRgb ({r,g,b,a=_} : rgba) : rgb = { r = r, g = g, b = b }

  fun maxR (a, b) : real = if a > b then a else b
  fun minR (a, b) : real = if a < b then a else b

  (* Hue from rgb given max, min channels and chroma. *)
  fun hueOf (r, g, b, mx, c) =
    if Real.== (c, 0.0) then 0.0
    else
      let
        val h =
          if Real.== (mx, r) then Real.rem ((g - b) / c, 6.0)
          else if Real.== (mx, g) then (b - r) / c + 2.0
          else (r - g) / c + 4.0
      in
        wrapHue (h * 60.0)
      end

  fun rgbToHsv ({r,g,b} : rgb) : hsv =
    let
      val mx = maxR (r, maxR (g, b))
      val mn = minR (r, minR (g, b))
      val c = mx - mn
      val s = if Real.== (mx, 0.0) then 0.0 else c / mx
    in
      { h = hueOf (r, g, b, mx, c), s = s, v = mx }
    end

  fun hsvToRgb ({h,s,v} : hsv) : rgb =
    let
      val h = wrapHue h
      val c = v * s
      val hp = h / 60.0
      val x = c * (1.0 - Real.abs (Real.rem (hp, 2.0) - 1.0))
      val (r1, g1, b1) =
        if hp < 1.0 then (c, x, 0.0)
        else if hp < 2.0 then (x, c, 0.0)
        else if hp < 3.0 then (0.0, c, x)
        else if hp < 4.0 then (0.0, x, c)
        else if hp < 5.0 then (x, 0.0, c)
        else (c, 0.0, x)
      val m = v - c
    in
      { r = r1 + m, g = g1 + m, b = b1 + m }
    end

  fun rgbToHsl ({r,g,b} : rgb) : hsl =
    let
      val mx = maxR (r, maxR (g, b))
      val mn = minR (r, minR (g, b))
      val c = mx - mn
      val l = (mx + mn) / 2.0
      val s =
        if Real.== (c, 0.0) then 0.0
        else c / (1.0 - Real.abs (2.0 * l - 1.0))
    in
      { h = hueOf (r, g, b, mx, c), s = s, l = l }
    end

  fun hslToRgb ({h,s,l} : hsl) : rgb =
    let
      val h = wrapHue h
      val c = (1.0 - Real.abs (2.0 * l - 1.0)) * s
      val hp = h / 60.0
      val x = c * (1.0 - Real.abs (Real.rem (hp, 2.0) - 1.0))
      val (r1, g1, b1) =
        if hp < 1.0 then (c, x, 0.0)
        else if hp < 2.0 then (x, c, 0.0)
        else if hp < 3.0 then (0.0, c, x)
        else if hp < 4.0 then (0.0, x, c)
        else if hp < 5.0 then (x, 0.0, c)
        else (c, 0.0, x)
      val m = l - c / 2.0
    in
      { r = r1 + m, g = g1 + m, b = b1 + m }
    end

  fun srgbToLinear u =
    if u <= 0.04045 then u / 12.92
    else Math.pow ((u + 0.055) / 1.055, 2.4)
  fun linearToSrgb u =
    if u <= 0.0031308 then u * 12.92
    else 1.055 * Math.pow (u, 1.0 / 2.4) - 0.055

  fun rgbToLinear ({r,g,b} : rgb) : rgb =
    { r = srgbToLinear r, g = srgbToLinear g, b = srgbToLinear b }
  fun rgbToSrgb ({r,g,b} : rgb) : rgb =
    { r = linearToSrgb r, g = linearToSrgb g, b = linearToSrgb b }

  (* --- CIE L*a*b* (D65) --- *)

  val pi = 3.14159265358979323846
  fun degToRad d = d * pi / 180.0
  fun radToDeg r = r * 180.0 / pi

  (* D65 white point in the same [0,1]-Y scale the matrices below produce. *)
  val xn = 0.95047
  val yn = 1.0
  val zn = 1.08883

  (* CIE Lab nonlinearity and its inverse. delta = 6/29. *)
  val labDelta  = 6.0 / 29.0
  val labDelta3 = labDelta * labDelta * labDelta            (* (6/29)^3 *)
  fun labF t =
    if t > labDelta3 then Math.pow (t, 1.0 / 3.0)
    else t / (3.0 * labDelta * labDelta) + 4.0 / 29.0
  fun labFinv t =
    if t > labDelta then t * t * t
    else 3.0 * labDelta * labDelta * (t - 4.0 / 29.0)

  fun toLab (c : rgb) : lab =
    let
      val {r, g, b} = rgbToLinear c
      val x = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
      val y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
      val z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b
      val fx = labF (x / xn)
      val fy = labF (y / yn)
      val fz = labF (z / zn)
    in
      { l = 116.0 * fy - 16.0, a = 500.0 * (fx - fy), b = 200.0 * (fy - fz) }
    end

  fun fromLab ({l, a, b} : lab) : rgb =
    let
      val fy = (l + 16.0) / 116.0
      val fx = fy + a / 500.0
      val fz = fy - b / 200.0
      val x = xn * labFinv fx
      val y = yn * labFinv fy
      val z = zn * labFinv fz
      val r =  3.2404542 * x - 1.5371385 * y - 0.4985314 * z
      val g = ~0.9692660 * x + 1.8760108 * y + 0.0415560 * z
      val bch = 0.0556434 * x - 0.2040259 * y + 1.0572252 * z
    in
      clampRgb (rgbToSrgb { r = r, g = g, b = bch })
    end

  fun labToLch ({l, a, b} : lab) : lch =
    let
      val c = Math.sqrt (a * a + b * b)
      val h = if Real.== (c, 0.0) then 0.0 else wrapHue (radToDeg (Math.atan2 (b, a)))
    in
      { l = l, c = c, h = h }
    end

  fun lchToLab ({l, c, h} : lch) : lab =
    let val hr = degToRad h
    in { l = l, a = c * Math.cos hr, b = c * Math.sin hr } end

  fun toLch c = labToLch (toLab c)
  fun fromLch lch = fromLab (lchToLab lch)

  fun deltaE76 ({l=l1,a=a1,b=b1} : lab, {l=l2,a=a2,b=b2} : lab) =
    let val dl = l1 - l2 and da = a1 - a2 and db = b1 - b2
    in Math.sqrt (dl * dl + da * da + db * db) end

  fun deltaE (c1, c2) = deltaE76 (toLab c1, toLab c2)

  fun deltaE2000 ({l=l1,a=a1,b=b1} : lab, {l=l2,a=a2,b=b2} : lab) =
    let
      fun pow7 x = Math.pow (x, 7.0)
      val c1 = Math.sqrt (a1 * a1 + b1 * b1)
      val c2 = Math.sqrt (a2 * a2 + b2 * b2)
      val cbar = (c1 + c2) / 2.0
      val cbar7 = pow7 cbar
      val g = 0.5 * (1.0 - Math.sqrt (cbar7 / (cbar7 + pow7 25.0)))
      val a1p = (1.0 + g) * a1
      val a2p = (1.0 + g) * a2
      val c1p = Math.sqrt (a1p * a1p + b1 * b1)
      val c2p = Math.sqrt (a2p * a2p + b2 * b2)
      fun hp (a, b) =
        if Real.== (a, 0.0) andalso Real.== (b, 0.0) then 0.0
        else wrapHue (radToDeg (Math.atan2 (b, a)))
      val h1p = hp (a1p, b1)
      val h2p = hp (a2p, b2)
      val dLp = l2 - l1
      val dCp = c2p - c1p
      val zeroChroma = Real.== (c1p * c2p, 0.0)
      val dhp =
        if zeroChroma then 0.0
        else
          let val d = h2p - h1p
          in if Real.abs d <= 180.0 then d
             else if d > 180.0 then d - 360.0
             else d + 360.0
          end
      val dHp = 2.0 * Math.sqrt (c1p * c2p) * Math.sin (degToRad (dhp / 2.0))
      val Lbarp = (l1 + l2) / 2.0
      val Cbarp = (c1p + c2p) / 2.0
      val hbarp =
        if zeroChroma then h1p + h2p
        else if Real.abs (h1p - h2p) <= 180.0 then (h1p + h2p) / 2.0
        else if (h1p + h2p) < 360.0 then (h1p + h2p + 360.0) / 2.0
        else (h1p + h2p - 360.0) / 2.0
      val t = 1.0
              - 0.17 * Math.cos (degToRad (hbarp - 30.0))
              + 0.24 * Math.cos (degToRad (2.0 * hbarp))
              + 0.32 * Math.cos (degToRad (3.0 * hbarp + 6.0))
              - 0.20 * Math.cos (degToRad (4.0 * hbarp - 63.0))
      val dTheta = 30.0 * Math.exp (~(Math.pow ((hbarp - 275.0) / 25.0, 2.0)))
      val Cbarp7 = pow7 Cbarp
      val Rc = 2.0 * Math.sqrt (Cbarp7 / (Cbarp7 + pow7 25.0))
      val dL50 = Lbarp - 50.0
      val Sl = 1.0 + (0.015 * dL50 * dL50) / Math.sqrt (20.0 + dL50 * dL50)
      val Sc = 1.0 + 0.045 * Cbarp
      val Sh = 1.0 + 0.015 * Cbarp * t
      val Rt = ~(Math.sin (degToRad (2.0 * dTheta))) * Rc
      val tL = dLp / Sl
      val tC = dCp / Sc
      val tH = dHp / Sh
    in
      Math.sqrt (tL * tL + tC * tC + tH * tH + Rt * tC * tH)
    end

  fun approxLab eps ({l=l1,a=a1,b=b1} : lab, {l=l2,a=a2,b=b2} : lab) =
    Real.abs (l1-l2) <= eps andalso Real.abs (a1-a2) <= eps
    andalso Real.abs (b1-b2) <= eps

  fun toByte x = Word32.fromInt (Real.round (clamp01 x * 255.0))

  fun pack ({r,g,b,a} : rgba) : Word32.word =
    let
      open Word32
    in
      orb (<< (toByte r, 0w24),
        orb (<< (toByte g, 0w16),
          orb (<< (toByte b, 0w8), toByte a)))
    end

  fun unpack (w : Word32.word) : rgba =
    let
      open Word32
      fun chan shift = Real.fromInt (toInt (andb (>> (w, shift), 0wxFF))) / 255.0
    in
      { r = chan 0w24, g = chan 0w16, b = chan 0w8, a = chan 0w0 }
    end

  (* --- hex --- *)
  fun hexDigit c =
    if Char.isDigit c then SOME (Char.ord c - Char.ord #"0")
    else
      let val lc = Char.toLower c
      in if lc >= #"a" andalso lc <= #"f"
         then SOME (Char.ord lc - Char.ord #"a" + 10)
         else NONE
      end

  fun hexPair (a, b) =
    case (hexDigit a, hexDigit b) of
        (SOME x, SOME y) => SOME (x * 16 + y)
      | _ => NONE

  fun byteToReal n = Real.fromInt n / 255.0

  fun fromHex s0 =
    let
      val s = if String.size s0 > 0 andalso String.sub (s0, 0) = #"#"
              then String.extract (s0, 1, NONE) else s0
      val cs = String.explode s
      fun nib c = Option.map (fn d => d * 16 + d) (hexDigit c)
      fun mk (r, g, b, a) =
        Option.map (fn ((rr,gg),(bb,aa)) =>
          { r = byteToReal rr, g = byteToReal gg,
            b = byteToReal bb, a = byteToReal aa })
          (case (r, g, b, a) of
             (SOME r, SOME g, SOME b, SOME a) => SOME ((r,g),(b,a))
           | _ => NONE)
    in
      case cs of
          [r,g,b] => mk (nib r, nib g, nib b, SOME 255)
        | [r,g,b,a] => mk (nib r, nib g, nib b, nib a)
        | [r1,r2,g1,g2,b1,b2] =>
            mk (hexPair (r1,r2), hexPair (g1,g2), hexPair (b1,b2), SOME 255)
        | [r1,r2,g1,g2,b1,b2,a1,a2] =>
            mk (hexPair (r1,r2), hexPair (g1,g2), hexPair (b1,b2), hexPair (a1,a2))
        | _ => NONE
    end

  fun byteToHex n =
    let
      val s = String.map Char.toLower (Int.fmt StringCvt.HEX n)
    in
      (if String.size s < 2 then "0" ^ s else s)
    end

  fun toHex ({r,g,b,a} : rgba) =
    let
      fun bv x = Real.round (clamp01 x * 255.0)
    in
      "#" ^ byteToHex (bv r) ^ byteToHex (bv g)
          ^ byteToHex (bv b) ^ byteToHex (bv a)
    end
  fun toHexRgb ({r,g,b} : rgb) =
    let fun bv x = Real.round (clamp01 x * 255.0)
    in "#" ^ byteToHex (bv r) ^ byteToHex (bv g) ^ byteToHex (bv b) end

  fun lerp ({r=r1,g=g1,b=b1,a=a1} : rgba, {r=r2,g=g2,b=b2,a=a2} : rgba, t) : rgba =
    { r = r1 + (r2 - r1) * t, g = g1 + (g2 - g1) * t,
      b = b1 + (b2 - b1) * t, a = a1 + (a2 - a1) * t }
  fun mix (c1, c2, t) = lerp (c1, c2, clamp01 t)

  fun approx eps ({r=r1,g=g1,b=b1,a=a1} : rgba, {r=r2,g=g2,b=b2,a=a2} : rgba) =
    Real.abs (r1-r2) <= eps andalso Real.abs (g1-g2) <= eps
    andalso Real.abs (b1-b2) <= eps andalso Real.abs (a1-a2) <= eps
  fun approxRgb eps ({r=r1,g=g1,b=b1} : rgb, {r=r2,g=g2,b=b2} : rgb) =
    Real.abs (r1-r2) <= eps andalso Real.abs (g1-g2) <= eps
    andalso Real.abs (b1-b2) <= eps
end
