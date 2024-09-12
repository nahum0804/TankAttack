import tkinter as tk

class TankAttackVentana:
    def __init__(self, root):
        self.root = root
        self.root.title("Tank Attack")
        self.root.geometry("800x600")
        self.label = tk.Label(self.root, text="Tank Attack", font=("Helvetica", 24, "bold"), fg="white", bg="black")
        self.label.pack(expand=True)
        self.root.after(4000, self.mostrar_nivel)

    def mostrar_nivel(self):
        self.label.config(text="Level 1")
        self.root.after(4000, self.mostrar_juego)

    def mostrar_juego(self):
        self.label.config(text="¡Comienza el juego!")
        self.root.after(4000, self.mostrar_pantalla_juego)
    
    def mostrar_pantalla_juego(self):
        self.label.destroy()
        self.root.config(bg="brown")

if __name__ == "__main__":
    root = tk.Tk()
    app = TankAttackVentana(root)
    root.mainloop()