import scipy.stats as st
import tkinter
#function
def cal():
    x=float(label_entryz.get())
    b=round(st.norm.cdf(x,0,1),5)
    label_result['text'] = f"the probablity is :{b}"


root=tkinter.Tk()
root.iconbitmap('fe.jpg')
root.title("Calculator Z score")
root.geometry('650x200')
#create GUI
label_z=tkinter.Label(root,text="enter the Z score:",font=("arial italic", 18))
label_z.grid(row=0,column=1)

label_entryz=tkinter.Entry(root,font=("arial italic", 18))
label_entryz.grid(row=0,column=2)

button_calculate = tkinter.Button(root, text="Calculate", command=cal,font=("arial italic", 18) )
button_calculate.grid(column=2, row=2)

label_result = tkinter.Label(root, text="the Probability is: ",font=("arial italic", 18))
label_result.grid(column=2, row=3)




# while True:

#     x=float(input("please insert the Z score value:"))
#     a=st.norm.cdf(x,0,1)
#     print(a)
   
root.mainloop()
   