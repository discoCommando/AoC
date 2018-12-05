module E2 exposing (..)

import Dict
import Helpers as H
import List
import String as S


main =
    H.makeMain [ res1 |> toString, res2 |> toString, ress ]


type St
    = Dict Char Int


pI =
    input |> String.lines |> List.map String.toList


c ch r =
    r
        |> Dict.update ch
            (\x ->
                case x of
                    Nothing ->
                        Just 1

                    Just a ->
                        Just (a + 1)
            )


cc d =
    d
        |> Dict.toList
        |> List.foldl
            (\( k, v ) ( dd, tw, th ) ->
                case v of
                    2 ->
                        case Dict.get 2 dd of
                            Nothing ->
                                ( dd |> Dict.insert 2 k, 1, th )

                            Just _ ->
                                ( dd, tw, th )

                    3 ->
                        case Dict.get 3 dd of
                            Nothing ->
                                ( dd |> Dict.insert 3 k, tw, 1 )

                            Just _ ->
                                ( dd, tw, th )

                    _ ->
                        ( dd, tw, th )
            )
            ( Dict.empty, 0, 0 )


res1 =
    pI |> List.map (List.foldl c Dict.empty >> Debug.log "asd" >> cc >> (\( _, tw, th ) -> ( tw, th ))) |> List.unzip |> (\( a, b ) -> List.sum a * List.sum b)


comp a1 a2 =
    case a1 of
        [] ->
            List.length a2

        aa1 :: r1 ->
            case a2 of
                [] ->
                    List.length a1

                aa2 :: r2 ->
                    case aa1 == aa2 of
                        True ->
                            comp r1 r2

                        False ->
                            1 + comp r1 r2


f x d =
    pI
        |> List.map
            (\a ->
                case comp x a of
                    0 ->
                        ( a, 1000000 )

                    x ->
                        ( a, x )
            )
        |> List.sortBy H.ts
        |> List.head
        |> H.uM
        |> (\r -> d |> Dict.insert x r)


diff a1 a2 =
    case a1 of
        [] ->
            []

        aa1 :: r1 ->
            case a2 of
                [] ->
                    []

                aa2 :: r2 ->
                    case aa1 == aa2 of
                        True ->
                            aa1 :: diff r1 r2

                        False ->
                            diff r1 r2


res2 =
    pI |> List.foldl f Dict.empty |> Dict.toList |> List.sortBy (H.ts >> H.ts) |> List.head |> H.uM


ress =
    res2 |> (\( a, ( b, _ ) ) -> diff a b |> String.fromList)


input =
    """cvfueihajytpmrdkgsxfqplbxn
cbzueihajytnmrdkgtxfqplbwn
cvzucihajytomrdkgstfqplqwn
cvzueilajytomrdkgsxfqwnbwn
cvzueihajytomrdkgsgwqphbwn
wuzuerhajytomrdkgsxfqplbwn
cyzueifajybomrdkgsxfqplbwn
cvzueihajxtomrdkgpxfqplmwn
ivzfevhajytomrdkgsxfqplbwn
cvzueihajytomrdlgsxfqphbbn
uvzueihajjtomrdkgsxfqpobwn
cvzupihajytomrdkgsxfqplpwe
cvzueihajyvomrdkgsxfqplbrl
cczueihajytomrdkgsnfqpxbwn
cvzueigajytdmrdkgsxyqplbwn
cvzujihljytomrdkgsxuqplbwn
cvzueisajytomrddgsxkqplbwn
cvzneihajytomrdkgsgaqplbwn
cvzueihajytomrdkgsinmplbwn
cveueihajyromrdkgsxfqplown
cypueihajytotrdkgzxfqplbwn
cvzuoihajytomvdqgsxfqplbwn
cvzuekhejytwmrdkgsxfqplbwn
cvzseihajytomrdkgsxfqgmbwn
cvfuhihajytomrdkgsxfqplbwi
cvzueihujxtomrdkgsufqplbwn
cvzueihdjytomrdogsxfqplbwh
cvzueihdjyfohrdkgsxfqplbwn
cvtudihajytolrdkgsxfqplbwn
cvzueihajytymrdkgshzqplbwn
cvzuebhajytomxdkgsxfqplbwt
cvzulihajyxomrdkgsbfqplbwn
cvzueihajywomrdkgsxfqplbts
cvzueihajytouodkdsxfqplbwn
cvzueihajytomgdkgqxfqklbwn
cvzubihajytomvdkgsxfqplmwn
cvhueihajyyocrdkgsxfqplbwn
zvzueihajytourdkgsxflplbwn
cvzbeihajytomadkgsxfoplbwn
cvzueihajytomrdkgnxfqplbsl
cvfueihajftkmrdkgsxfqplbwn
cvzuexhajytomryugsxfqplbwn
cvzueihajytomsckgsxfqalbwn
cvzuexhajytomrdkbsxfqpluwn
cvzueihajytbmrtkgsxwqplbwn
cvzueihajytomrdigsxfqqlbsn
cvzweihajytomydkgsxfmplbwn
bvzteihajytimrdkgsxfqplbwn
cvzueihajytpmrdkgsxfcpbbwn
cvzueigsjltomrdkgsxfqplbwn
cvzueihajytomrikgsxfopldwn
cvzueihajstomrdkgsxfqplgon
cvzueimajytomrnkxsxfqplbwn
cvzleihagatomrdkgsxfqplbwn
cvbueihajotomrdkgsxfqjlbwn
cvzueihajytomrdkgsxfqppnvn
hvzueihajytomrdkghxfkplbwn
cvzueigajytxmrdkgsxfqplbjn
cvzueihaaxtokrdkgsxfqplbwn
cvzueihajyeomrdkgujfqplbwn
cvzueiwajpoomrdkgsxfqplbwn
cvzieidtjytomrdkgsxfqplbwn
cvzueihalytomrakbsxfqplbwn
wtzueihajytomrdkgsxfqplbwq
cvzuelhaiytomrdkgsxfqplcwn
cvzueihajytomrdkgsxfqslswd
cvzueihajytomrykgssfqplbon
cvzueihfjytovrdegsxfqplbwn
cvzueihajytomldqgsxfqplbwy
cvzleihjjytomrtkgsxfqplbwn
cvzueihaldtomrdtgsxfqplbwn
cvzueihajytzmrdkgsxfeplqwn
cvzueihrjytomddkgsxfqpgbwn
cyzulihajytokrdkgsxfqplbwn
cvsueihajytoordfgsxfqplbwn
fvzueyhajytomrdkgaxfqplbwn
cczueihajytobrdkgsefqplbwn
cvzueihajytomcdrgscfqplbwn
cvzuexhajyvomrdkgssfqplbwn
cvzsmihajyiomrdkgsxfqplbwn
cvzzeihajttomrdkgsxzqplbwn
cvzseihajytomrdkgsxfqpebvn
cvzueihajgthmrdkgsbfqplbwn
ruzueihajytomrdkgsxfqphbwn
cvzueihajytofrdkgsnfrplbwn
cvzuetdajytojrdkgsxfqplbwn
fvzueihajytomrdkghxfqpobwn
cvzueihsjytomrdkgsxfqglbxn
cvzueihajytowrdkgsxfqpsbun
cvzteihaiytomrdkfsxfqplbwn
cvzueihajytkmrdkrsxfqplvwn
cvzueihajyoomrdkasxfqjlbwn
lvzurihajytkmrdkgsxfqplbwn
cvzueihajyyomrdagsxfqelbwn
cvfueihajytomrdkgsxfqplbbx
cvwueihajytommdkgkxfqplbwn
cvzucicajytomrdkgsxcqplbwn
dvzueihahytgmrdkgsxfqplbwn
cvzuechajytomrdkgsxfqelwwn
cvzuekhajytomrdkgsxknplbwn
cvtueihajytomphkgsxfqplbwn
cvzueihabytnzrdkgsxfqplbwn
cvzusihajytomrdkgfxfqplban
cvfueihajytomcdfgsxfqplbwn
mvzueihapytomrdkgsxfdplbwn
cvzueihajytomhdkgsxmqppbwn
jvsueihajytomrdkgsxfqplbln
cvzujihajybomrdkgsxtqplbwn
cvzuekhawytomrdkgsxfqplbwc
svzueihanytomrdogsxfqplbwn
cvzujihajytodrdkgslfqplbwn
cvgdeihajytorrdkgsxfqplbwn
cvzbeihajytoprdkgsxfqplbyn
cvzueihkyytomjdkgsxfqplbwn
cvzuelhojytomrdkgsxfqjlbwn
evzueihajytimrdkgsxfqpsbwn
cvzueihajydomrdkjsxfqplbjn
ovzteihajytosrdkgsxfqplbwn
cvzueihajyaomrdzgsxfqplbgn
cvzuewhajmtomrdkgsufqplbwn
cvzueihajqtomhukgsxfqplbwn
cvzueihajytomzqkgsxfqplbwk
cazuewhakytomrdkgsxfqplbwn
clzueihatytomrdkgzxfqplbwn
dvzueihajytomqdkgsxfqpnbwn
cvzueidajdtomrdkgsxtqplbwn
cvzueihabytowrdkgsxoqplbwn
cvzujihwjytomrdkgsxeqplbwn
cvtuedhajytomrdkgsxfqplbbn
cvzueihajcgomrdkgsxfqplswn
cvzuephajyiomrdngsxfqplbwn
cvzueihajythmqdkgsxfqplbwf
cvzueitajytomrdkgsxfepvbwn
cvzueihajytomydkgsxfqplvwb
dvzueshajytomrddgsxfqplbwn
cvzueihajytomrdkgvxfqpwben
cvzueihajytomrdkgvxfpplwwn
cvzuefhajftomrdkgsxfqrlbwn
cvzueihajytpmrvkgsxfqplbcn
cvzueihajytohrdkgsxfqxnbwn
cvzueihajytomrdposxfqulbwn
cozueihajytomrpkgsxfqrlbwn
cvzuuihaxytomrdkgsxfqplbtn
cvzueihajytomrbzgsxyqplbwn
cveueihajyxoqrdkgsxfqplbwn
cvzueihajytomrkkgsxfqptbrn
cvzuezhajatomrdkssxfqplbwn
cpzueihajytomrdkgsxfhplbwo
lviueihajytomrekgsxfqplbwn
cvzueihwjytomrdkusxfyplbwn
cvzgeihajytomwdkgsxfrplbwn
cvzsejhzjytomrdkgsxfqplbwn
cvzuuihajytomrdkgsxfqdlbwz
cvzjeihajytomrdugsxftplbwn
cvzueihaxytomrrkgsxfmplbwn
cvzueihajgtomrdhgsxfqplwwn
cvzulihajytomedkgsxfqplewn
cvzueivajytomrdkmsxfqplbwc
cvzuervajytomrdkgsxfwplbwn
cvzuemhcjytomrdkgslfqplbwn
cvzyerhauytomrdkgsxfqplbwn
cvzueihaoytomrdkgsyfqplewn
cvzueihanytomrdkgsafkplbwn
cvzueihajvtomrdugsxfqpcbwn
chzueihajytamrdxgsxfqplbwn
cvzueihalytomrdsgsxfqplbln
cvzueihajytoyaykgsxfqplbwn
tlzueihajyeomrdkgsxfqplbwn
cvpueihajytbmrdkgsxfxplbwn
cvzueihajytomjdkgsxuqplkwn
cvzueihajygomrdkgkxfqplbwg
cvzueihajhtomrdkgbxsqplbwn
cvzurihajytomrdkgsafqplbwx
cdzuezhajytomrdkgsxrqplbwn
cvbueihajytotrwkgsxfqplbwn
cwzkeihajytomrdkgsxfqplbwh
cvzheihajytolrikgsxfqplbwn
cozuevhajytomrdkgkxfqplbwn
chzueihajytomrjkgsxfqulbwn
cvzueihkjyromrdkgsxvqplbwn
cvzveihajytomrdkgsxpqplnwn
cvzueihajytoirdkgsxfqihbwn
cvoueihajytomrdkgsxfqpdawn
pvzueihajytomrdkgnxfqplbfn
cvzueihakytomxdkgssfqplbwn
cvzueivajytomrdbgsxaqplbwn
cvzueihajytokrdkgszrqplbwn
cvzuevhajytomrdkgsxgqplbwi
cvzueihajylomrdkgsxflplbpn
hvzueihajytomvdkgsxfqplgwn
cvzleihajytymrrkgsxfqplbwn
crzueieajytomrdkgsxfqplbon
cszueihajytomrdlgqxfqplbwn
cvzueihacytomrdkgsxfjblbwn
cvzreihajytomrdkgsxfqplzun
cvzurihajytomrdkgsxiqplawn
uvzueihajyhovrdkgsxfqplbwn
cvzueihajyqodrdkgssfqplbwn
cvzwiihrjytomrdkgsxfqplbwn
cqzueihajytomrdkgjxfqplban
cvmueihajytoordkgsxfqplbyn
cypueihajytomrdkgzxfqplbwn
cvzueihajykomrdkgsmfqplbtn
cvzueidajytimrdkgsxfqpdbwn
cvzheihajytomrdkgsxfqpfewn
dvzueihajytumrdzgsxfqplbwn
cvzueixajytomrdkgsvfqplgwn
cvzuevhzjyzomrdkgsxfqplbwn
cvyeeihajytomrdkgsxnqplbwn
cvzueihajytomrdkggtpqplbwn
cvzceiyajytomrdkgexfqplbwn
cvzuelhajyyomrdkzsxfqplbwn
cvzhzihajygomrdkgsxfqplbwn
cvzueihwjytomrdkgsgfqplbrn
cvzsevhajytomrdkgqxfqplbwn
cvzueiuajytomrdkgsxfppebwn
nvzueihajytemrdkgsxwqplbwn
cvzueihajytocgdkgsxfqvlbwn
cczusihajytomrdkgsxfqplbpn
cmzueihajytomrdkbsxwqplbwn
cvzumfdajytomrdkgsxfqplbwn
cvzueihcjytomrdkgsxfqplbkl
cvzueihajytomawknsxfqplbwn
kvzueihijytomrdkgsxdqplbwn
cdzutihajytomrdkgsxfkplbwn
cvzufihadylomrdkgsxfqplbwn
cvzueihajytomrgkxsxfqphbwn
cvzuewhajyzomrdkgsxfqelbwn
cvzueihajytomrdkgqxfqelbwc
cvzueshajyoomrdkgsxfqflbwn
cvzueihajyromrekgixfqplbwn
chzugihajytomrdkgsxfqplawn
cvzueihajytomrdkgsxfhpmbwy
cvzueihacytodxdkgsxfqplbwn
cvzurihajytourdkgsdfqplbwn
cvzzeihmjytomrddgsxfqplbwn
cvzucyhajygomrdkgsxfqplbwn
ckzueihzjytomrdkgsxwqplbwn
cvlueihajmtozrdkgsxfqplbwn
cvzkeihajytomrdkgsxfqclbwc
cvzueihajytomrdkgsxgdplbwa
cvzueihyjytoxrdkgcxfqplbwn
cvzueizavytomfdkgsxfqplbwn
cvzueihajwtosrdkgsxfqllbwn
cvzueihajytomrdaksxfqpllwn
cvzuuihojytombdkgsxfqplbwn
cvzuiibajytpmrdkgsxfqplbwn
cvzueihajyuomydkgsxfqplzwn
cvzueihajytimrmkgsxfqplfwn
cvzueihajytomrdkgzxfqpljwo"""
