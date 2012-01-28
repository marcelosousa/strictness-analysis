let name f = fun f' => fun x => f' x in 
    let name x' = f (fun x => 4) 9
    in  f (fun x => x) x' end
  end