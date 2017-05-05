(* based closely on the paper "Strictly Pretty" by Christian Lindig 
  *
  * URL: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=A7B1EF1668A1983E747286BB1A68FD19?doi=10.1.1.34.2200&rep=rep1&type=pdf
*)
module PP : 
sig 
  type 'a doc
  type tag = string
  val empty : 'a doc
  val (^^) : 'a doc -> 'a doc -> 'a doc
  val nestRelative : int -> 'a doc -> 'a doc
  val nestAbsolute : int -> 'a doc -> 'a doc
  val text : string * 'a -> 'a doc 
  val tagged : tag -> 'a doc -> 'a doc
  val blockBoundary : 'a doc
  val optionalBreak : string * 'a -> 'a doc
  val mandatoryBreak : 'a doc

  type 'a sdoc = SEmpty 
               | SText of string * 'a * 'a sdoc
               | STagStart of tag * 'a sdoc
               | STagEnd of 'a sdoc
               | SLine of int * 'a sdoc
  val sdoc_of_doc : int -> 'a doc -> 'a sdoc
  val string_of_sdoc : 'a sdoc -> string

  type 'a id_table = (string, 'a) Hashtbl.t
  val html_of_sdoc : 'a sdoc -> 
    ([> Html_types.div] Tyxml_js.Html5.elt) * 'a id_table
end = 
struct
  type tag = string
  type 'a doc = Empty
              | Concat of 'a doc * 'a doc
              | NestRelative of int * 'a doc
              | NestAbsolute of int * 'a doc
              | Text of string * 'a
              | TagStart of tag
              | TagEnd (* internal invariant: TagStart and TagEnd are always matched *)
              | BlockBoundary 
              | OptionalBreak of string * 'a
              | MandatoryBreak
  let empty = Empty
  let (^^) x y = Concat (x, y)
  let nestRelative n x = NestRelative (n, x)
  let nestAbsolute n x = NestAbsolute (n, x)
  let text (s, data) = Text (s, data)
  let tagged tag x = Concat (TagStart tag, Concat (x, TagEnd))
  let blockBoundary = BlockBoundary
  let optionalBreak (s, data) = OptionalBreak (s, data) 
  let mandatoryBreak = MandatoryBreak

  type 'a sdoc = SEmpty 
               | SText of string * 'a * 'a sdoc
               | STagStart of tag * 'a sdoc
               | STagEnd of 'a sdoc
               | SLine of int * 'a sdoc

  let strlen = CamomileLibrary.UTF8.length
  (* let strlen = String.length *) 

  let rec sdoc_of_doc' width k zs = match zs with 
    | [] -> SEmpty
    | (i, x) :: zs' -> 
      begin match x with 
        | Empty -> sdoc_of_doc' width k zs'
        | Concat (x1, x2) -> 
          sdoc_of_doc' width k ((i, x1) :: (i, x2) :: zs')
        | NestRelative (n, x') -> 
          sdoc_of_doc' width k (((n + k), x') :: zs') 
        | NestAbsolute (n, x') ->
          sdoc_of_doc' width k (((n + i), x') :: zs')
        | Text (s, data) -> 
          SText (s, data, sdoc_of_doc' width (k + (strlen s)) zs')
        | TagStart tag -> 
          STagStart (tag, sdoc_of_doc' width k zs')
        | TagEnd -> 
          STagEnd (sdoc_of_doc' width k zs')
        | BlockBoundary -> 
          if i == k then sdoc_of_doc' width k zs' 
          else SLine (i, sdoc_of_doc' width i zs')
        | OptionalBreak (s, data) -> 
          if (width - k) <= 0 
          then 
            SLine (i, sdoc_of_doc' width i zs')
          else
            SText (s, data, sdoc_of_doc' width (k + (strlen s)) zs')
        | MandatoryBreak -> 
          SLine (i, sdoc_of_doc' width i zs')
      end 
  let sdoc_of_doc width x = sdoc_of_doc' width 0 [(0, x)]

  let rec string_of_sdoc x = match x with 
    | SEmpty -> ""
    | SText (s, _, x') -> s ^ (string_of_sdoc x')
    | STagStart (tag, x') -> string_of_sdoc x'
    | STagEnd x' -> string_of_sdoc x'
    | SLine(n, x') -> "\n" ^ (String.make n ' ') ^ (string_of_sdoc x')

  let last_id = ref 0
  let fresh_id () = 
    let i = !last_id in 
    last_id := i + 1;
    "hazel_id_" ^ (string_of_int i)

  type 'a id_table = (string, 'a) Hashtbl.t
  open Tyxml_js

  let rec html_of_sdoc'' x table = 
    begin match x with 
      | SEmpty -> ([], None)
      | SText (s, data, x') -> 
        let span_id = fresh_id () in 
        let _ = Hashtbl.add table span_id data in 
        let (h, x'') = html_of_sdoc'' x' table in 
        let h' = (Html5.(span ~a:[a_class ["SText"]; a_id span_id]
                           [pcdata s])) :: h in 
        (h', x'')
      | STagStart (tag, x') -> 
        let (h, x'') = html_of_sdoc'' x' table in 
        let (tl, rem) = 
          begin match x'' with 
            | Some x'' -> 
              html_of_sdoc'' x'' table 
            | None -> ([], None)
          end in 
        let h' = (Html5.(span ~a:[a_class [tag]] h)) :: tl in 
        (h', rem)
      | STagEnd x' -> 
        ([], Some x')
      | SLine (n, x') -> 
        let newline = Html5.br () in 
        let indentation = Html5.(span ~a:[a_class ["SIndentation"]]
                                   [pcdata (String.make n ' ')]) in 
        let (tl, rem) = html_of_sdoc'' x' table in 
        let h = newline :: indentation :: tl in 
        (h, rem)
    end
  let rec html_of_sdoc x = 
    let table = Hashtbl.create 256 in 
    let (h, _) = html_of_sdoc'' x table in 
    (Html5.(div ~a:[a_class ["SDoc"]] h), table)
end 



