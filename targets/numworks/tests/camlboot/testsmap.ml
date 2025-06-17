open Data

let () =
  (* Création d'une map vide *)
  let map = SMap.empty in

  (* Ajout d'éléments *)
  let map = SMap.add "key1" "value1" map in
  let map = SMap.add "key2" "value2" map in

  (* Vérification de la présence d'une clé *)
  
  assert (SMap.mem "key1" map);
  assert (not (SMap.mem "key3" map));

  (* Récupération d'une valeur *)
  let value1 = SMap.find "key1" map in
  assert (value1 = "value1");

  (* Mise à jour d'une valeur *)
  let map = SMap.add "key1" "new_value1" map in
  let updated_value1 = SMap.find "key1" map in
  assert (updated_value1 = "new_value1");

  (* Suppression d'une clé *)
  let map = SMap.remove "key1" map in
  assert (not (SMap.mem "key1" map));

  (* Itération sur les éléments *)
  SMap.iter (fun key value ->
    print_string "Key: ";
    print_string key;
    print_string "Value: ";
    print_endline value;
  ) map;

  (* Taille de la map *)
  let size = SMap.cardinal map in
  assert (size = 1);

  (* Conversion en liste *)
  let bindings = SMap.bindings map in
  assert (bindings = [("key2", "value2")]);

  print_endline "Tous les tests SMap ont réussi !"