#include <iostream>

/* arbres binaires de recherche */

class ABR {
public:
  int valeur;
  ABR *gauche;
  ABR *droite;
  ABR(ABR *g, int v, ABR *d);
  void insere(int x);
  int contient(int x);
  void affiche();
};

ABR::ABR(ABR *g, int v, ABR *d) { valeur = v; gauche = g; droite = d; }

void ABR::insere(int x) {
  if (x == valeur) return;
  if (x < valeur) {
    if (gauche == NULL)
      gauche = new ABR(NULL, x, NULL);
    else
      gauche->insere(x);
  } else
    if (droite == NULL)
      droite = new ABR(NULL, x, NULL);
    else
      droite->insere(x);
}

int ABR::contient(int x) {
  if (x == valeur) return true;
  if (x < valeur && gauche != NULL) return gauche->contient(x);
  if (droite != NULL) return droite->contient(x);
  return false;
}

void ABR::affiche() {
  if (gauche != NULL) gauche->affiche();
  std::cout << "(" << valeur << ")";
  if (droite != NULL) droite->affiche();
}


int main() {
  ABR dico = ABR(NULL, 1, NULL);
  dico.insere(17);
  dico.insere(5);
  dico.insere(8);
  dico.affiche(); std::cout << "\n";

  if (dico.contient(5) &&
      ! dico.contient(0) &&
      dico.contient(17) &&
      ! dico.contient(3))
    std::cout << "ok\n";

  dico.affiche(); std::cout << "\n";
}
