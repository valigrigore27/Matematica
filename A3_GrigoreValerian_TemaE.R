E1=function(ipoteza, x, miu, s, n, alfa)
{
  t_score = (x - miu) / (s / sqrt(n))
  if (ipoteza == 'left') {
    c_t = qt(alfa, n - 1)
    if (t_score > c_t) {
      print("Se accepta ipoteza nula")
    }
    else {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'right') {
    critical_t = qt(1 - alfa, n - 1)
    print(paste(c_t))
    if (t_score < c_t) {
      print("Se accepta ipoteza nula")
    }
    else {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == "sim") {
    c_t = qt(1 - alfa / 2, n - 1)
    print(paste(c_t))
    if (abs(t_score) < abs(c_t)) 
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
}
E1('left', 418, 420, 2.75, 125, 0.01)

E2=function(ipoteza, x, miu0, sigma, n, alfa)
{
  z_score = (x - miu0) / (sigma / sqrt(n))
  print(z_score)
  if (ipoteza == 'left') {
    c_z = qnorm(alfa, 0, 1)
    if (z_score > c_z) {
      print("Se accepta ipoteza nula")
    }
    else {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'right') {
    c_z = qnorm(1 - alfa, 0, 1)
    if (z_score < c_z) {
      print("Se accepta ipoteza nula")
    }
    else {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'sim') {
    c_z = qnorm(1 - alfa / 2, 0, 1)
    if (abs(z_score) < abs(c_z)) {
      print("Se accepta ipoteza nula")
    }
    else {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
}
E2('right', 5.17, 4.9, 0.35, 25, 0.01)
E2('right', 5.17, 4.9, 0.35, 25, 0.05)

E3 = function(ipoteza, x1, x2, n1, n2, sigma1, sigma2, m0, alfa) 
{
  z_score = (x1 - x2 -m0) / sqrt(sigma1 ^ 2 / n1 + sigma2 ^ 2 / n2)
  if (ipoteza == 'left') 
  {
    c_z = qnorm(alfa, 0, 1)
    if (z_score > c_z)
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'right')
  {
    c_z = qnorm(1 - alfa, 0, 1)
    if (z_score < c_z) 
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'sim') 
  {
    c_z = qnorm(1 - alfa / 2, 0, 1)
    if (abs(z_score) < abs(c_z)) 
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
}
E3('sim', 5.48, 6.12, 25, 28, 1.31, 0.93, 0, 0.01)
E3('left', 5.48, 6.12, 25, 28, 1.31, 0.93, 0, 0.01)

E4 = function(ipoteza,s1,s2,n1,n2,alfa)
{
  F_score = s1 ^ 2 / s2 ^ 2
  if (ipoteza == 'sim')
  {
    c_F_s = qf(alfa/2, n1-1, n2-1)
    c_F_d = qf(1-alfa/2, n1 - 1, n2 - 1)
    if (F_score > c_F_s  || F_score < c_F_d)
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'right')
  {
    c_F_d = qf(1-alfa, n1 - 1, n2 - 1)
    if (F_score < c_F_d)
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
  if (ipoteza == 'left')
  {
    c_F_s = qf(alfa, n1 - 1, n2 - 1)
    if (F_score > c_F_s)
    {
      print("Se accepta ipoteza nula")
    }
    else 
    {
      print("Se respinge ipoteza nula si se accepta ipoteza alternativa \n")
    }
  }
}
E4('right',1.24,0.87,25,28,0.01)