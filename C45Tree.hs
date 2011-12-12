module C45Tree(
    
    )where

import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
    
-- | Тип для атрибута. Атрибут должен быть перечислимого типа, иметь максимальное и минимальное значение,
-- | и "непрерывно" изменяться между ними    
data (Enum a) => Attribute a = A {
    aName :: String,
    minValue :: a,
    maxValue :: a
    }
    
instance (Enum a) => Eq (Attribute a) where
    (==) = (==) `on` aName    
    
instance (Enum a) => Show (Attribute a) where
    show = aName
    
-- | Тип для дерева принятия решения. а - тип атрибутов. b - тип меток (классов) 
data (Enum a) => DecisionTree a b = Leaf b                -- ^ В листе  - метка
        | Node {               
            attr  :: Attribute a,                         -- ^ Атрибут по которому происходит разделение в узле
            threshold :: a,                               -- ^ Значение по которому происходит разделение
            child :: a -> (DecisionTree a b)              -- ^ Функция возвращающая потомка по значению атрибута
            }

instance (Enum a, Show a, Show b) => Show (DecisionTree a b) where
    show tree = showTree tree ""
    
showTree :: (Enum a, Show a, Show b) => DecisionTree a b -> ShowS
showTree (Leaf x) = shows x
showTree (Node attr threshold child) = ('<':).shows attr.("|\n"++).showList [child threshold,child$succ threshold].('>':) 
    
-- | Тип для классифицируемых объектов
data (Enum a) => Datum a = D {
        dName :: String, -- ^ Объект может иметь имя 
        attrs :: [(Attribute a,a)] -- ^ Список пар (аттрибут, значение), описывающих объект
        } deriving Show
        
-- | Синоним типа для пары (Класс, Оъект)
type PreLabeled a b= (b, Datum a)        


-- | Функция строит дерево решения по списку атрибутов и списку классифицированных объектов    
--build :: (Enum a) => [Attribute a] -> [PreLabeled a b] -> DecisionTree a b

-- | Функция дает оценку количества информауции, необходимого для определения класса примера из разбиения
infoX :: Ord b => [Prelabeled a b] -> [Prelabeled a b] -> Double
infoX x y = nx / n * info x + ny / n * info y 
    where nx = fromIntegral $ length x
          ny = fromIntegral $ length ny
          n = nx + ny

-- | Функция дает оценку колиества информации, необходимого для определения класса примера из множества
-- Строго говоря логарифм должен быть двоичный, тогда получим количество информации в битах.
-- Но так как количество информации дл конкретного множество нас интересует не по абсолютной величине, а лишь в сравнении
-- с другими, можно обойтись и натуральным логарифмом
info ::Ord b =>  [PreLabeled a b] -> Double
info set = (-1)*(Map.fold f 0 $ groupLabels $ map fst set)
    where n = fromIntegral $ length set
          f s acc | s/=0 = acc + p * log p
                  | otherwise = error "Невозможная ошибка в функции info"
                        where p = fromIntegral s / n
                  
-- | Значение атрибута у объекта
getValue ::Enum a =>  Datum a-> Attribute a ->  a
getValue d attr = fromJust $ lookup attr (attrs d)    


-- Функция подсчитывает количество повторений каждого класса в списке.
groupLabels :: Ord b =>  [b] -> Map b Int 
groupLabels = groupWith id (const (1::Int)) (const succ)

-- | группирует список в отображение (Map)
groupWith ::Ord k =>   (a -> k) -- ^ функция получения ключа элемента
    -> (a -> v) -- ^ преобразование элмента списка к элементу отображения
    -> (v -> v -> v) -- ^ как поступать с двумя элементами с одинаковым ключом
    -> [a] -- ^ группируемый список
    -> Map k v 
groupWith getKey singleton fuse = 
    foldl (\m x -> Map.insertWith fuse (getKey x) (singleton x) m) Map.empty
    
    
 

