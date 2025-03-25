package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import u03.extensionmethods.Sequences.Sequence.{Cons, nil}

class SchoolModelTest:

  val module: SchoolModule = BasicSchoolModule

  import module.*

  @Test def testTeacher() =
    assertEquals("Ghini", module.teacher("Ghini"))

  @Test def testCourse() =
    assertEquals("PPS", module.course("PPS"))

  @Test def testEmptySchool() =
    assertEquals(nil(), module.emptySchool)


  @Test def testSetCourseTeacher() =
    val teacher: Teacher = module.teacher("Ghini")
    val course: Course = module.course("OOP")

    assertEquals(Cons(("Ghini", "OOP"), nil()), module.emptySchool.setTeacherToCourse(teacher, course))
    assertEquals(Cons(("Ghini", "OOP"), Cons(("Ghini", "PPS"), nil())), module.emptySchool.setTeacherToCourse(teacher, course).setTeacherToCourse(module.teacher("Ghini"), module.course("PPS")))

  @Test def testCoursesOfTeacher() =
    assertEquals(Cons("OOP", Cons("PPS", nil())), module.emptySchool.setTeacherToCourse(teacher("Ghini"), course("OOP")).setTeacherToCourse(teacher("Ghini"), course("PPS")).coursesOfATeacher(module.teacher("Ghini")))
