object MyMain {
    def temp(t: Int) = (t * 9 / 5) + 32

    def itoMon(m: Int): String = m match {
        case 1 => "Jan"
        case 2 => "Feb"
        case _ => "Other"
    }

    def printDate(dt: String) = {
        val dtPattern = raw"(\d+)/(\d+)/(\d+)".r
            val dtPattern(myDay, myMon, myYear) = dt
            println(myDay + " " + itoMon(myMon.toInt) + " " + myYear)
            (myDay, myMon, myYear)
    }

    def parseUnixUser(line: String) = {
        //  "root:x:0:0:/root:/bin/sh"
        val unixPW = raw"(.*):(.*):(.*):(.*):(.*):(.*)".r
            var unixPW(u, p, uid, gid, homedirectory, shell) = line
            (u, p, uid, gid, homedirectory, shell)
    }


    class Person ( val firstName: String,
            val lastName: String,
            val age: Int
            ) {
        // op1: change age from 'val' to 'var'
        // op2: remove val age, add dob property
    }

    class UnixUser private (val uname: String,
            val pw: String,
            val shell: String
            ) {
        println(s"creating an UnixUser ${shell}")

        def hasFullLogin = shell.toLowerCase().endsWith("sh")
    }

    object UnixUser {
        var count = 0
            def apply(line: String): UnixUser = {
                val tmp = parseUnixUser(line)
                    count += 1
                    new UnixUser(tmp._1, tmp._2, tmp._6)
            }
    }

    def gcd(a: Int, b: Int): Int = {
        if (a == 0) {
            b
        } else {
            gcd(b % a, a)
        }
    }

    def lcm(a: Int, b: Int) = {
        a / gcd(a, b) * b
    }

    class Fraction(val numerator: Int,
                   val denominator: Int
                   ) {
        assert (denominator != 0)

        def + (that: Fraction) = {
            val vlcm = lcm(this.denominator, that.denominator)
            val newN = vlcm / this.denominator * that.numerator
                       + vlcm / that.denominator * that.numerator
            new Fraction(newN, vlcm)
        }
    }

    trait Ordered[A] extends java.lang.Comparable[A] {
        def < (that: A) = compareTo(that) < 0
        def > (that: A) = compareTo(that) > 0
    }

    class OrdFraction(n: Int, d: Int)
        extends Fraction(n, d) with Ordered[OrdFraction] {
            // TODO
            def compareTo(that: OrdFraction): Int = 0 //this + that
    }

    trait Priviliged {
        def priviliges: Array[String]
    }

    // LAB 5
    val makeFunc = (op: String) => op match {
        case "add" => ( (a:Int, b:Int) => a+b)
        case "sub" => ( (a:Int, b:Int) => a-b)
    }

    // LAB 6



    def main(args: Array[String]):Unit = {
        val m = itoMon(1)
            println (m)
            val ( m1, d1, y1) = printDate("01/01/2016")
            val dd = parseUnixUser("root:x:0:0:/root:/bin/sh")
            val unixUser = UnixUser("root:x:0:0:/root:/bin/sh")
            val unixUser1 = UnixUser("root:x:0:0:/root:/bin/python")
            println(unixUser.hasFullLogin)
            println(unixUser1.hasFullLogin)

        // val f0 = new Fraction(1, 0)
        val ordFraction = new OrdFraction(1, 1)
        // val privUnixUser = UnixUser("root:x:0:0:/root:/bin/sh") { def prililiges: Array[String] = ("p1", "p2") }
        println(makeFunc("add")(3, 4))
        println(makeFunc("sub")(3, 4))

        println("=== LAB 6 ===")

        val files = (new java.io.File(".")).listFiles
        val nonHidenFileNames = files.map( _.getName()).filter( !_.startsWith("."))
        nonHidenFileNames.foreach( println(_))

        val parts = files.filter(!_.getName().startsWith(".")).partition(_.isDirectory())
        parts._1.foreach( i => println(i.getName()))
        parts._2.foreach( i => println(i.getName()))

        val fileLength = files.map( i => i.length())
        val fileNameLengthPairs = files.map( i => i.getName() ).zip(fileLength)
        fileNameLengthPairs.foreach( println(_._1 + _._2.toString()))

        // println(nameSizePairs._1)
        // nameSizePairs.toList.foreach( print(_) )
    }
}
