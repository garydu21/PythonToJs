# Projet PythonToJS — L3 Info, Types de données et preuves 2025-2026 - BESANCENEZ Angel - KALOUCHE Chris

## Compilation

```
dune build
```

## Exécution

Lecture d'un fichier Python, affichage de l'AST et génération JavaScript :
```
dune exec PythonToJS f <fichier.py>
```

Génération d'une page HTML complète (avec champs input et bouton Compute) :
```
dune exec PythonToJS h <fichier.py> > result.html
```

Lecture interactive depuis la console :
```
dune exec PythonToJS i
```

## Tests

Les fichiers de test se trouvent dans le répertoire `test/` :

- `evenodd.py` — programme is_even/is_odd, fonctions mutuellement récursives, déclaration inline, appels imbriqués
- `test_new.py` — couvre Cond, While, VardeclS dans le corps de fonction, CallS au niveau global
- `test_fonctions.py` — typage de fonctions avec paramètres, type de retour, variables locales
- `test_html.py` — génération HTML basique avec un input et un print
- `test_html_full.py` — génération HTML avec valeur_absolue (Cond) et compte (While)

Le sous-répertoire `test/grammaire/` contient les tests pour la grammaire :

- `add.py`, `minus.py`, `mult.py`, `div.py`, `mod.py` — opérations arithmétiques
- `comp.py`, `comp_bool.py` — comparaisons et opérations booléennes
- `decv.py` — déclarations de variables
- `assignement.py` — affectations
- `if.py` — conditionnelles
- `while.py` — boucles
- `fonction.py`, `test_appel_fonction.py` — définitions et appels de fonctions

## Résumé du travail effectué

### Analyse lexicale et syntaxique (section 2.1)

Le parser Menhir fourni par le professeur a été utilisé et étendu. Il gère les blocs délimités par les commentaires `#begin` et `#end` (section 2.1.1). La grammaire couvre les définitions de fonctions avec annotations de types, les déclarations de variables, les affectations, les conditionnelles `if/else`, les boucles `while`, les instructions `return`, et les appels de fonctions et procédures.

### Vérification de types (section 2.2)

Le fichier `typing.ml` implémente la vérification de types décrite dans le sujet :

- **Expressions** (`tp_expr`) : typage des constantes, variables, opérations binaires arithmétiques, booléennes et de comparaison, et appels de fonction. La vérification d'appel utilise `tp_compatible` qui vérifie l'inclusion de types union.
- **Instructions** (`tp_stmt`) : gestion de `Block`, `VardeclS` (déclarations inline dans le corps), `Assign` avec vérification du type statique déclaré, `Cond` avec fusion des environnements dynamiques des deux branches, `While` avec calcul de point fixe par itération jusqu'à stabilisation, `Return` avec accumulation du type de retour, `CallS`.
- **Fonctions** (`tp_fundefn`) : construction de l'environnement local avec paramètres et déclarations locales, vérification du corps, comparaison du type de retour effectif avec le type déclaré.
- **Programme** (`tp_prog`) : construction de l'environnement initial avec les signatures de toutes les fonctions et les déclarations globales, vérification de toutes les fonctions puis des instructions globales.

Les environnements statique et dynamique sont distincts : le statique correspond aux déclarations, le dynamique est mis à jour à chaque affectation et permet de vérifier les opérations binaires avec les types effectifs.

### Traduction vers JavaScript (section 2.3)

Le fichier `pprinter.ml` implémente la génération de code à l'aide de la bibliothèque PPrint :

- `def` devient `function`, `or`/`and` deviennent `||`/`&&`, les annotations de type sont supprimées
- `VardeclS` génère `let v;`, `While` génère `while(...){}`, `Cond` génère `if(...){} else {}`
- Les fonctions dans le corps sont indentées de 4 espaces

**Génération HTML** (annexe D) : l'option `h` génère une page HTML complète où les fonctions sont placées dans un `<script>`, les `input` deviennent des `<input type="number">` avec label, et le `print` devient un bouton Compute qui affiche le résultat. Les valeurs des champs sont converties avec `parseInt()` pour éviter la concaténation de chaînes en JavaScript (différence sémantique Python/JS identifiée).

### Différences sémantiques Python/JS identifiées

- **Opérateur `+`** : en Python `+` sur deux strings est une concaténation ; en JS `.value` d'un input retourne toujours une string, donc `"5" + "5" = "55"`. Solution : `parseInt()` sur les valeurs des champs.
- **Division entière** : Python a `//` pour la division entière, JS n'a qu'un seul type numérique.
- **Booléens** : Python `True`/`False`, JS `true`/`false`.
- **`and`/`or`** : en Python ces opérateurs retournent l'une des valeurs (pas nécessairement un booléen), en JS `&&`/`||` ont un comportement similaire mais avec coercition de type différente.

## Ce qui n'a pas pu être fait

- **Opérations arithmétiques sur types mixtes** : `tp_expre_binOp` ne gère que `int op int` pour l'arithmétique. Les promotions `bool < int < float` et la concaténation `str + str` ne sont pas vérifiées.
- **Vérification du type de retour dans Cond/While** : le type de retour accumulé dans les branches n'est pas parfaitement propagé.

## Ce qui est particulièrement bien réussi

- La génération HTML suit exactement le schéma de l'annexe D du sujet et produit des pages directement ouvrables dans un navigateur.
- Le calcul de point fixe pour `While` est implémenté et fonctionne correctement.
- La distinction environnement statique / dynamique est bien gérée, avec mise à jour correcte dans locals ou globals selon le contexte.

## Utilisation d'assistants IA

Claude (Anthropic) a été utilisé comme assistant tout au long du projet, notamment pour la compréhension des consignes, vérifier si certaines parties du code étaient correctes, et l'élaboration des fichiers de test.
