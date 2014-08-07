
guard :shell do
  watch /.*\.hs$/ do |m|
    %x{
      ghc --make Main.hs
      runhaskell Test.hs
    }
  end
end
