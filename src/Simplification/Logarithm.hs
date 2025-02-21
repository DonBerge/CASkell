{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Simplification.Logarithm
-- Description :  Simplificación de expresiones con funciones logaritmicas.
--
-- Sean \(u,v\) y \(w\) expresiones, la función logaritmo satisface las siguientes propiedades:
--
-- \[
--     \begin{align*}
--         \exp(u+v) &= \exp(u) \cdot \exp(v) \\
--         \exp(w \cdot u) &= \exp(u)^w \\
--     \end{align*}
-- \]
--
-- La operacion que aplica las propiedades de izquierda a derecha se llama /expansión exponencial/ y la operación que las aplica de derecha a izquierda
-- se llama /contracción exponencial/. Este modulo contiene definiciones tanto para la expansión como para la contracción de exponenciales.

module Simplification.Logarithm where
    