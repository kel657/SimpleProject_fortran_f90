Program Function_graph
Implicit None
character(50) :: string
Real x , y
integer, parameter :: tabl =6
Open(tabl,File='OUT.txt')!контрольный текстовый файл

string = "Координаты горизонтали"
Write(tabl,*) string 
call TextOutput(string)
Write(*,*) '(-7,3) ==> (-3,3)'
Write(tabl,*) '(-7,3) ==> (-3,3)'
string = "координаты дуги окружности"
Write(tabl,*) string 
call TextOutput(string)
Do x=-3,3,1
y = x*x/3
Write(*,*)' (x,y)=', x,y 
Write(tabl,*)' (x,y)=', x,y 
End do
string = "координаты прямой проходящей через две точки"
Write(tabl,*)string
call TextOutput(string)
Do x= 3, 6, 1
y = 9 - 2*x
Write(*,*)' (x,y)=',x,y
Write(tabl,*)' (x,y)=',x,y
End do
string = "Координаты прямой"
Write(tabl,*)string
call TextOutput(string)
Do x=6, 11, 1
y = x-9
Write(*,*)' (x,y)=',x,y
Write(tabl,*)' (x,y)=',x,y
End do

End Program Function_graph

subroutine TextOutput(string1)
character(50), intent(inout) :: string1
integer(2) :: i, dos_win_code, dif
do i = 1, len_trim(string1)
dos_win_code = iachar(string1(i:i))
select case(dos_win_code)
case(32) !если пробел
dif = 0
case(192 : 239) ! Windows-русские буквы от А до Я и от а до п
dif = -64
case(240 : 255) ! Windows-русские буквы от р до я
dif = -16
end select
string1(i:i) = char(dos_win_code + dif)
end do
print *,string1
end subroutine
