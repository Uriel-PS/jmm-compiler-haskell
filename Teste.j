.class public teste
.super java/lang/Object

.method public <init>()V
	aload_0
	invokenonvirtual java/lang/Object/<init>()V
	return
.end method

.method public static maior(DD)D
	.limit stack 30
	.limit locals 5

	dload 0
	dload 2
	dcmpg
	ifgt l0
	goto l1
l0:
	dload 0
	d2i
	istore 4
	goto l2
l1:
	dload 2
	d2i
	istore 4
l2:
	iload 4
	i2d
	dreturn
.end method

.method public static fat(I)I
	.limit stack 30
	.limit locals 2

	iconst_0
	istore 1
l3:
	iload 0
	iconst_0
	if_icmpgt l4
	goto l5
l4:
	iload 1
	iload 0
	imul
	istore 1
	iload 0
	iconst_1
	isub
	istore 0
	goto l3
l5:
	iload 1
	ireturn
.end method

.method public static somatorio(I)I
	.limit stack 30
	.limit locals 4

	iconst_0
	i2d
	dstore 2
	iconst_0
	istore 1
l6:
	iload 1
	iload 0
	if_icmplt l7
	goto l8
l7:
	dload 2
	iload 1
	i2d
	dadd
	dstore 2
	iload 1
	iconst_1
	iadd
	istore 1
	goto l6
l8:
	dload 2
	d2i
	ireturn
.end method

.method public static imprimir(Ljava/lang/String;D)V
	.limit stack 30
	.limit locals 3

	getstatic java/lang/System/out Ljava/io/PrintStream;
	aload 0
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	getstatic java/lang/System/out Ljava/io/PrintStream;
	dload 1
	invokevirtual java/io/PrintStream/println(D)V
	return
	return
.end method

.method public static main([Ljava/lang/String;)V
	.limit stack 30
	.limit locals 5

	getstatic java/lang/System/out Ljava/io/PrintStream;
	ldc "Numero:"
	invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
	new java/util/Scanner
	dup
	getstatic java/lang/System/in Ljava/io/InputStream;
	invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
	invokevirtual java/util/Scanner/nextInt()I
	istore 2
	ldc2_w 4.5
	d2i
	invokestatic teste/fat(I)I
	istore 1
	ldc2_w 2.5
	bipush 10
	i2d
	invokestatic teste/maior(DD)D
	dstore 3
	ldc "teste:"
	iconst_1
	i2d
	invokestatic teste/imprimir(Ljava/lang/String;D)V
	return
	return
.end method

