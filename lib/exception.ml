exception Fatal of string

let fatal s = raise (Fatal s)
