package org.skyve.impl.domain.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import java.util.Calendar;

import org.junit.Test;
import org.skyve.domain.types.TimeOnly;

@SuppressWarnings("static-method")
public class TimeOnlyUserTypeTest {

	@Test
	public void testHashCodeReturnsSameValueForSameTime() {
		TimeOnlyUserType userType = new TimeOnlyUserType();
		TimeOnly t1 = new TimeOnly(10, 30, 0);
		TimeOnly t2 = new TimeOnly(10, 30, 0);
		assertEquals(userType.hashCode(t1), userType.hashCode(t2));
	}

	@Test
	public void testHashCodeReturnsDifferentValueForDifferentTime() {
		TimeOnlyUserType userType = new TimeOnlyUserType();
		TimeOnly t1 = new TimeOnly(10, 30, 0);
		TimeOnly t2 = new TimeOnly(11, 45, 0);
		assertNotEquals(userType.hashCode(t1), userType.hashCode(t2));
	}

	@Test
	public void testEqualsReturnsTrueForSameHMSFromDifferentDays() {
		TimeOnlyUserType userType = new TimeOnlyUserType();
		// Create two dates with the same H/M/S/MS but different day — different epoch ms but equal per equals()
		Calendar cal1 = Calendar.getInstance();
		cal1.set(2024, Calendar.JANUARY, 1, 10, 30, 0);
		cal1.set(Calendar.MILLISECOND, 0);
		Calendar cal2 = Calendar.getInstance();
		cal2.set(2024, Calendar.JANUARY, 2, 10, 30, 0);
		cal2.set(Calendar.MILLISECOND, 0);
		// Different epochs but same H/M/S/MS → equals should return true (covers L56 true branch)
		org.junit.Assert.assertTrue(userType.equals(cal1.getTime(), cal2.getTime()));
	}

	@Test
	public void testEqualsReturnsFalseForDifferentMilliseconds() {
		TimeOnlyUserType userType = new TimeOnlyUserType();
		// Same H/M/S but different millis → equals returns false (covers L56 false branch)
		Calendar cal1 = Calendar.getInstance();
		cal1.set(2024, Calendar.JANUARY, 1, 10, 30, 0);
		cal1.set(Calendar.MILLISECOND, 0);
		Calendar cal2 = Calendar.getInstance();
		cal2.set(2024, Calendar.JANUARY, 1, 10, 30, 0);
		cal2.set(Calendar.MILLISECOND, 500);
		org.junit.Assert.assertFalse(userType.equals(cal1.getTime(), cal2.getTime()));
	}
}
