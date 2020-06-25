# Datos del ejercicio
sigmay = 11/8
sigmaz = 2
beta12 = 0.25
beta21 = 0
a11 = 0.5
a12 = 0.1
a21 = -0.4
a22 = 0.5

# FIR 

A = matrix(nrow = 2, ncol = 2)
A[1,1] = a11
A[1,2] = a12
A[2,1] = a21
A[2,2] = a22
B = matrix(nrow = 2, ncol = 2)
B[1,1] = 1
B[1,2] = beta12
B[2,1] = beta21
B[2,2] = 1

    
Psi0 = solve(B)
Psi1 = A%*%Psi0
Psi2 = A%*%Psi1


e = matrix(nrow = 2, ncol = 2)
e[1,1]
e[1,2]
e[2,1]
e[2,2]

# DESCOMPOSICIÓN DE VARIANZAS
# Caso 1 beta21 = 0 -------------------------------------------------------

sigmay = 11/8
sigmaz = 2
beta12 = 0.25
beta21 = 0


psi0 = matrix(nrow = 2, ncol = 2)
psi1 = matrix(nrow = 2, ncol = 2)
psi2 = matrix(nrow = 2, ncol = 2)

psi0[1,1] = 1
psi0[1,2] = -beta12
psi0[2,1] = -beta21
psi0[2,2] = 1

psi1[1,1] = a11 - a12*beta21
psi1[1,2] = -a11*beta12 + a12
psi1[2,1] = a21 - a22*beta21
psi1[2,2] = -a21*beta12 + a22

psi2[1,1] = a11^2 + a12*a21
psi2[1,2] = -beta12*(a11^2 + a12*a21) + a11*a12 + a22*a12
psi2[2,1] = a21*a11 + a22*a21
psi2[2,2] = -beta12*(a21*a11+a22*a21) + a21*a12 + a22^2

sigmayS1 = sigmay*(sum(psi0[1,1]^2)) + sigmaz*(sum(psi0[1,2]^2))
sigmayZ1 = sigmay*(sum(psi0[2,1]^2)) + sigmaz*(sum(psi0[2,2]^2))

sigmayS2 = sigmay*(sum(psi1[1,1]^2)) + sigmaz*(sum(psi1[1,2]^2))
sigmayZ2 = sigmay*(sum(psi1[2,1]^2)) + sigmaz*(sum(psi1[2,2]^2))

VD1yy = (sigmay*psi0[1,1]^2)/sigmayS1*100
VD1yz = (sigmaz*psi0[1,2]^2)/sigmayS1*100
VD1zz = (sigmaz*psi0[2,2]^2)/sigmayZ1*100
VD1zy = (sigmaz*psi0[2,1]^2)/sigmayZ1*100

VD1yy = (sigmay*psi1[1,1]^2)/sigmayS2*100
VD1yz = (sigmaz*psi1[1,2]^2)/sigmayS2*100
VD1zz = (sigmaz*psi1[2,2]^2)/sigmayZ2*100
VD1zy = (sigmay*psi1[2,1]^2)/sigmayZ2*100


# Caso 2 beta12=0 ---------------------------------------------------------

sigmay = 11/8
sigmaz = 2
beta12 = 0.0
beta21 = 1/3


psi0 = matrix(nrow = 2, ncol = 2)
psi1 = matrix(nrow = 2, ncol = 2)
psi2 = matrix(nrow = 2, ncol = 2)

psi0[1,1] = 1
psi0[1,2] = -beta12
psi0[2,1] = -beta21
psi0[2,2] = 1

psi1[1,1] = a11 - a12*beta21
psi1[1,2] = -a11*beta12 + a12
psi1[2,1] = a21 - a22*beta21
psi1[2,2] = -a21*beta12 + a22

psi2[1,1] = a11^2 + a12*a21
psi2[1,2] = -beta12*(a11^2 + a12*a21) + a11*a12 + a22*a12
psi2[2,1] = a21*a11 + a22*a21
psi2[2,2] = -beta12*(a21*a11+a22*a21) + a21*a12 + a22^2

sigmayS1 = sigmay*(sum(psi0[1,1]^2)) + sigmaz*(sum(psi0[1,2]^2))
sigmayZ1 = sigmay*(sum(psi0[2,1]^2)) + sigmaz*(sum(psi0[2,2]^2))

# Este esta mal, le falta psi0
sigmayS2 = sigmay*(sum(psi1[1,1]^2)) + sigmaz*(sum(psi1[1,2]^2))
sigmayZ2 = sigmay*(sum(psi1[2,1]^2)) + sigmaz*(sum(psi1[2,2]^2))

VD1yy = (sigmay*psi0[1,1]^2)/sigmayS1*100
VD1yz = (sigmaz*psi0[1,2]^2)/sigmayS1*100
VD1zz = (sigmaz*psi0[2,2]^2)/sigmayZ1*100
VD1zy = (sigmaz*psi0[2,1]^2)/sigmayZ1*100

VD1yy = (sigmay*psi1[1,1]^2)/sigmayS2*100
VD1yz = (sigmaz*psi1[1,2]^2)/sigmayS2*100
VD1zz = (sigmaz*psi1[2,2]^2)/sigmayZ2*100
VD1zy = (sigmay*psi1[2,1]^2)/sigmayZ2*100


