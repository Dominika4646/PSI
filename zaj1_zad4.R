# 4. Stwórz funkcję, która oblicza długość 
# przeciwprostokątnej w trójkącie prostokątnym.
# z braku informacji w poleceniu zakladam ze obliczamy ja 
# na podstawie przyprostokatknych

przeciwprostokatna = function(przyprostokatna1, przyprostokatna2) {
  suma = przyprostokatna1^2 + przyprostokatna2^2
  dlugosc = sqrt(suma)
  return (dlugosc)
}

przeciwprostokatna(3,4)