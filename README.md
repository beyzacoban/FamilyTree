# Family Tree Management System

Bu proje, Prolog programlama dili kullanılarak geliştirilmiş bir aile ağacı yönetim sistemidir.

## Özellikler

- Kişi ekleme ve güncelleme
- Aile ilişkilerini sorgulama
- Aile ağacını görüntüleme
- Evlilik bilgilerini yönetme
- Detaylı kişi bilgilerini görüntüleme

## Desteklenen İlişkiler

- Anne-Baba
- Kardeş (Erkek/Kız)
- Amca-Hala
- Dayı-Teyze
- Yenge-Enişte
- Gelin-Damat
- Kayınvalide-Kayınpeder
- Baldız-Bacanak
- Yeğen
- Kuzen

## Kurulum

1. SWI-Prolog'u yükleyin: [SWI-Prolog İndirme Sayfası](https://www.swi-prolog.org/download/stable)
2. Projeyi klonlayın:
```bash
git clone https://github.com/beyzacoban/FamilyTree.git
```
3. Prolog konsolunda programı yükleyin:
```prolog
?- [aile].
```

## Kullanım

Programı başlatmak için:
```prolog
?- start.
```

Ana menü seçenekleri:
1. İlişki sorgulama
2. Kişi ekleme/güncelleme
3. Kişi bilgilerini görüntüleme
4. Aile ağacını yazdırma
5. Evlilik ekleme
6. Programı sonlandırma

## Örnek Kullanım

```prolog
?- start.
Family Tree Warehouse Application (FTWA)
--------------------------------------
Main Menu:
1. Ask relation
2. Add/update person
3. Get information of any person
4. Print the family tree
5. Add marriage
6. Terminate program

Please choose an operation (1-6): 
```
