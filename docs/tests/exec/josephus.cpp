#include <iostream>

/*** listes circulaires doublement chaînées ***/

class ListeC {
  public:
  int valeur;
  ListeC *suivant;
  ListeC *precedent;
  ListeC(int v);
  void insererApres(int v);
  void supprimer();
  void afficher();
};

/* constructeur = liste réduite à un élément */
ListeC::ListeC(int v) {
  valeur = v;
  suivant = precedent = this;
}

/* insertion après un élément */
void ListeC::insererApres(int v) {
  ListeC *e = new ListeC(0);
  e->valeur = v;
  e->suivant = suivant;
  suivant = e;
  e->suivant->precedent = e;
  e->precedent = this;
}

/* suppression d'un élément */
void ListeC::supprimer() {
  precedent->suivant = suivant;
  suivant->precedent = precedent;
}

/* affichage */
void ListeC::afficher() {
  ListeC *c = this;
  std::cout << c->valeur << " ";
  c = c->suivant;
  for (; c != this;) {
    std::cout << c->valeur << " ";
    c = c->suivant;
  }
  std::cout << "\n";
}

/*** problème de Josephus ***/

/* construction de la liste circulaire 1,2,...,n;
   l'élément retourné est celui contenant 1 */
ListeC *cercle(int n) {
  ListeC *l = new ListeC(1);
  int i;
  for (i = n; i >= 2; i--) {
    l->insererApres(i);
  }
  return l;
}

/* jeu de Josephus */
int josephus(int n, int p) {
  /* c est le joueur courant, 1 au départ */
  ListeC *c = cercle(n);

  /* tant qu'il reste plus d'un joueur */
  for (; c != c->suivant;) {
    /* on élimine un joueur */
    int i;
    for (i = 1; i < p; i++)
      c = c->suivant;
    c->supprimer();
    // std::cout << c->valeur << " est éliminé";
    c = c->suivant;
  }
  // std::cout << "le gagnant est " << c->valeur;
  return c->valeur;
}

/*** Tests ***/

int main() {
  ListeC l = ListeC(1);
  l.afficher();
  l.insererApres(3);
  l.afficher();
  l.insererApres(2);
  l.afficher();
  l.suivant->supprimer();
  l.afficher();

  ListeC *c = cercle(7);
  c->afficher();

  if (josephus(7, 5) == 6 &&
      josephus(5, 5) == 2 &&
      josephus(5, 17) == 4 && josephus(13, 2) == 11)
    std::cout << "ok\n";
}
