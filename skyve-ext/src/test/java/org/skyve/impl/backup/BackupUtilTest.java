package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.skyve.impl.backup.BackupUtil.redactDateOnly;
import static org.skyve.impl.backup.BackupUtil.redactDateTime;
import static org.skyve.impl.backup.BackupUtil.redactGeometry;
import static org.skyve.impl.backup.BackupUtil.redactNumeric;
import static org.skyve.impl.backup.BackupUtil.redactString;
import static org.skyve.impl.backup.BackupUtil.redactTimeOnly;
import static org.skyve.impl.backup.BackupUtil.redactTimestamp;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import org.junit.Test;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

public class BackupUtilTest {

	@Test
	public void testRedactSensitiveData() {
		// setup the test data
		String input1 = "Client 1",
				input2 = "Cli2",
				input3 = "Cl3",
				input4 = "C4",
				input5 = "C",
				input6 = "",
				input7 = null,
				input8 = "Really long client name",
				input9 = "client1@email.org";

		// call the method under test
		String result1 = redactString(input1);
		String result2 = redactString(input2);
		String result3 = redactString(input3);
		String result4 = redactString(input4);
		String result5 = redactString(input5);
		String result6 = redactString(input6);
		String result7 = redactString(input7);
		String result8 = redactString(input8);
		String result9 = redactString(input9);
		
		// verify the result
		assertThat(result1, is("Cl****1"));
		assertThat(result2, is("C****2"));
		assertThat(result3, is("C****3"));
		assertThat(result4, is("C****"));
		assertThat(result5, is("****"));
		assertThat(result6, is(""));
		assertThat(result7, is(nullValue()));
		assertThat(result8, is("Re**********me"));
		assertThat(result9, is("cl****t1@em*****rg"));
	}
	
	@Test
	public void testRedactInteger() {
		// setup the test data
		Integer input1 = Integer.valueOf(0);
		Integer input2 = Integer.valueOf(5);
		Integer input3 = Integer.valueOf(50);
		Integer input4 = Integer.valueOf(-48);
		Integer input5 = Integer.valueOf(-1111);
		Integer input6 = Integer.valueOf(589);
		Integer input7 = Integer.valueOf(43);
		Integer input8 = Integer.valueOf(-91);
		Integer input9 = Integer.valueOf(160);
		Integer input10 = Integer.valueOf(-1);
		
		// call the method under test
		Integer result1 = redactNumeric(input1);
		Integer result2 = redactNumeric(input2);
		Integer result3 = redactNumeric(input3);
		Integer result4 = redactNumeric(input4);
		Integer result5 = redactNumeric(input5);
		Integer result6 = redactNumeric(input6);
		Integer result7 = redactNumeric(input7);
		Integer result8 = redactNumeric(input8);
		Integer result9 = redactNumeric(input9);
		Integer result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(Integer.valueOf(0)));
		assertThat(result2, is(Integer.valueOf(10)));
		assertThat(result3, is(Integer.valueOf(50)));
		assertThat(result4, is(Integer.valueOf(-50)));
		assertThat(result5, is(Integer.valueOf(-1110)));
		assertThat(result6, is(Integer.valueOf(590)));
		assertThat(result7, is(Integer.valueOf(40)));
		assertThat(result8, is(Integer.valueOf(-90)));
		assertThat(result9, is(Integer.valueOf(160)));
		assertThat(result10, is(Integer.valueOf(0)));
	}
	
	@Test
	public void testRedactLong() {
		// setup the test data
		Long input1 = Long.valueOf("30000000000");
		Long input2 = Long.valueOf("23179213");
		Long input3 = Long.valueOf("56245344543");
		Long input4 = Long.valueOf("123687126311");
		Long input5 = Long.valueOf("123126387123");
		Long input6 = Long.valueOf("1");
		Long input7 = Long.valueOf("-123712361");
		Long input8 = Long.valueOf("-123154356");
		Long input9 = Long.valueOf("-453454319");
		Long input10 = Long.valueOf("-8");
		
		// call the method under test
		Long result1 = redactNumeric(input1);
		Long result2 = redactNumeric(input2);
		Long result3 = redactNumeric(input3);
		Long result4 = redactNumeric(input4);
		Long result5 = redactNumeric(input5);
		Long result6 = redactNumeric(input6);
		Long result7 = redactNumeric(input7);
		Long result8 = redactNumeric(input8);
		Long result9 = redactNumeric(input9);
		Long result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(Long.valueOf("30000000000")));
		assertThat(result2, is(Long.valueOf("23179210")));
		assertThat(result3, is(Long.valueOf("56245344540")));
		assertThat(result4, is(Long.valueOf("123687126310")));
		assertThat(result5, is(Long.valueOf("123126387120")));
		assertThat(result6, is(Long.valueOf("0")));
		assertThat(result7, is(Long.valueOf("-123712360")));
		assertThat(result8, is(Long.valueOf("-123154360")));
		assertThat(result9, is(Long.valueOf("-453454320")));
		assertThat(result10, is(Long.valueOf("-10")));
	}
	
	@Test
	public void testRedactDecimal2() {
		// setup the test data
		Decimal2 input1 = new Decimal2("0.21");
		Decimal2 input2 = new Decimal2("12.61");
		Decimal2 input3 = new Decimal2("0.49");
		Decimal2 input4 = new Decimal2("4.51");
		Decimal2 input5 = new Decimal2("-0.21");
		Decimal2 input6 = new Decimal2("-9.471");
		Decimal2 input7 = new Decimal2("0.11");
		Decimal2 input8 = new Decimal2("91.91");
		Decimal2 input9 = new Decimal2("0.01");
		Decimal2 input10 = new Decimal2("843454.1");
		
		// call the method under test
		Decimal2 result1 = redactNumeric(input1);
		Decimal2 result2 = redactNumeric(input2);
		Decimal2 result3 = redactNumeric(input3);
		Decimal2 result4 = redactNumeric(input4);
		Decimal2 result5 = redactNumeric(input5);
		Decimal2 result6 = redactNumeric(input6);
		Decimal2 result7 = redactNumeric(input7);
		Decimal2 result8 = redactNumeric(input8);
		Decimal2 result9 = redactNumeric(input9);
		Decimal2 result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(new Decimal2("0")));
		assertThat(result2, is(new Decimal2("10")));
		assertThat(result3, is(new Decimal2("0")));
		assertThat(result4, is(new Decimal2("0")));
		assertThat(result5, is(new Decimal2("0")));
		assertThat(result6, is(new Decimal2("-10")));
		assertThat(result7, is(new Decimal2("0")));
		assertThat(result8, is(new Decimal2("90")));
		assertThat(result9, is(new Decimal2("0")));
		assertThat(result10, is(new Decimal2("843450")));
	}
	
	@Test
	public void testRedactDecimal5() {
		// setup the test data
		Decimal5 input1 = new Decimal5("0.212345254");
		Decimal5 input2 = new Decimal5("112.6123123");
		Decimal5 input3 = new Decimal5("5.123123");
		Decimal5 input4 = new Decimal5("41.556423423432");
		Decimal5 input5 = new Decimal5("-0.2123423342");
		Decimal5 input6 = new Decimal5("-129.4745324531");
		Decimal5 input7 = new Decimal5("023.11123123");
		Decimal5 input8 = new Decimal5("94531.9123121");
		Decimal5 input9 = new Decimal5("0454.01231231");
		Decimal5 input10 = new Decimal5("8445454.12131");
		
		// call the method under test
		Decimal5 result1 = redactNumeric(input1);
		Decimal5 result2 = redactNumeric(input2);
		Decimal5 result3 = redactNumeric(input3);
		Decimal5 result4 = redactNumeric(input4);
		Decimal5 result5 = redactNumeric(input5);
		Decimal5 result6 = redactNumeric(input6);
		Decimal5 result7 = redactNumeric(input7);
		Decimal5 result8 = redactNumeric(input8);
		Decimal5 result9 = redactNumeric(input9);
		Decimal5 result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(new Decimal5("0")));
		assertThat(result2, is(new Decimal5("110")));
		assertThat(result3, is(new Decimal5("10")));
		assertThat(result4, is(new Decimal5("40")));
		assertThat(result5, is(new Decimal5("0")));
		assertThat(result6, is(new Decimal5("-130")));
		assertThat(result7, is(new Decimal5("20")));
		assertThat(result8, is(new Decimal5("94530")));
		assertThat(result9, is(new Decimal5("450")));
		assertThat(result10, is(new Decimal5("8445450")));
	}
	
	@Test
	public void testRedactDecimal10() {
		// setup the test data
		Decimal10 input1 = new Decimal10("0.212342342345254");
		Decimal10 input2 = new Decimal10("12342.6123234223");
		Decimal10 input3 = new Decimal10("2345.123234123");
		Decimal10 input4 = new Decimal10("23141.5523234426423423432");
		Decimal10 input5 = new Decimal10("-1230.2123423342");
		Decimal10 input6 = new Decimal10("-123411230.2123423342");
		Decimal10 input7 = new Decimal10("-234234.143543");
		Decimal10 input8 = new Decimal10("94531.9123121231");
		Decimal10 input9 = new Decimal10("-0454.012312312");
		Decimal10 input10 = new Decimal10("8445454.12123131");
		
		// call the method under test
		Decimal10 result1 = redactNumeric(input1);
		Decimal10 result2 = redactNumeric(input2);
		Decimal10 result3 = redactNumeric(input3);
		Decimal10 result4 = redactNumeric(input4);
		Decimal10 result5 = redactNumeric(input5);
		Decimal10 result6 = redactNumeric(input6);
		Decimal10 result7 = redactNumeric(input7);
		Decimal10 result8 = redactNumeric(input8);
		Decimal10 result9 = redactNumeric(input9);
		Decimal10 result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(new Decimal10("0")));
		assertThat(result2, is(new Decimal10("12340")));
		assertThat(result3, is(new Decimal10("2350")));
		assertThat(result4, is(new Decimal10("23140")));
		assertThat(result5, is(new Decimal10("-1230")));
		assertThat(result6, is(new Decimal10("-123411230")));
		assertThat(result7, is(new Decimal10("-234230")));
		assertThat(result8, is(new Decimal10("94530")));
		assertThat(result9, is(new Decimal10("-0450")));
		assertThat(result10, is(new Decimal10("8445450")));
	}
	
	@Test
	public void testRedactDateOnly() {
		// setup the test data
		DateOnly input1 = new DateOnly(LocalDate.of(2000, 1, 15));
		DateOnly input2 = new DateOnly(LocalDate.of(1989, 6, 11));
		DateOnly input3 = new DateOnly(LocalDate.of(2000, 6, 23));
		DateOnly input4 = new DateOnly(LocalDate.of(2010, 4, 15));
		DateOnly input5 = new DateOnly(LocalDate.of(2020, 3, 2));
		DateOnly input6 = new DateOnly(LocalDate.of(2000, 11, 28));
		DateOnly input7 = new DateOnly(LocalDate.of(1990, 10, 25));
		DateOnly input8 = new DateOnly(LocalDate.of(1991, 10, 19));
		DateOnly input9 = new DateOnly(LocalDate.of(1972, 9, 6));
		DateOnly input10 = new DateOnly(LocalDate.of(1892, 11, 3));
		
		// call the method under test
		DateOnly result1 = redactDateOnly(input1);
		DateOnly result2 = redactDateOnly(input2);
		DateOnly result3 = redactDateOnly(input3);
		DateOnly result4 = redactDateOnly(input4);
		DateOnly result5 = redactDateOnly(input5);
		DateOnly result6 = redactDateOnly(input6);
		DateOnly result7 = redactDateOnly(input7);
		DateOnly result8 = redactDateOnly(input8);
		DateOnly result9 = redactDateOnly(input9);
		DateOnly result10 = redactDateOnly(input10);
		
		// verify the result
		assertThat(result1, is(new DateOnly(LocalDate.of(2000, 1, 1))));
		assertThat(result2, is(new DateOnly(LocalDate.of(1989, 6, 1))));
		assertThat(result3, is(new DateOnly(LocalDate.of(2000, 6, 1))));
		assertThat(result4, is(new DateOnly(LocalDate.of(2010, 4, 1))));
		assertThat(result5, is(new DateOnly(LocalDate.of(2020, 3, 1))));
		assertThat(result6, is(new DateOnly(LocalDate.of(2000, 11, 1))));
		assertThat(result7, is(new DateOnly(LocalDate.of(1990, 10, 1))));
		assertThat(result8, is(new DateOnly(LocalDate.of(1991, 10, 1))));
		assertThat(result9, is(new DateOnly(LocalDate.of(1972, 9, 1))));
		assertThat(result10, is(new DateOnly(LocalDate.of(1892, 11, 1))));
	}
	
	@Test
	public void testRedactDateTime() {
		// setup the test data
		DateTime input1 = new DateTime(LocalDateTime.of(2000, 1, 15, 23, 58));
		DateTime input2 = new DateTime(LocalDateTime.of(1989, 5, 11, 21, 45));
		DateTime input3 = new DateTime(LocalDateTime.of(2000, 8, 8, 18, 45));
		DateTime input4 = new DateTime(LocalDateTime.of(2010, 2, 1, 14, 19));
		DateTime input5 = new DateTime(LocalDateTime.of(2020, 9, 18, 11, 59));
		DateTime input6 = new DateTime(LocalDateTime.of(2000, 11, 23, 17, 22));
		DateTime input7 = new DateTime(LocalDateTime.of(1990, 12, 11, 19, 11));
		DateTime input8 = new DateTime(LocalDateTime.of(1991, 2, 12, 23, 36));
		DateTime input9 = new DateTime(LocalDateTime.of(1972, 3, 15, 18, 8));
		DateTime input10 = new DateTime(LocalDateTime.of(1892, 1, 14, 21, 5));
		
		// call the method under test
		DateTime result1 = redactDateTime(input1);
		DateTime result2 = redactDateTime(input2);
		DateTime result3 = redactDateTime(input3);
		DateTime result4 = redactDateTime(input4);
		DateTime result5 = redactDateTime(input5);
		DateTime result6 = redactDateTime(input6);
		DateTime result7 = redactDateTime(input7);
		DateTime result8 = redactDateTime(input8);
		DateTime result9 = redactDateTime(input9);
		DateTime result10 = redactDateTime(input10);
		
		// verify the result
		assertThat(result1, is(new DateTime(LocalDateTime.of(2000, 1, 1, 0, 0))));
		assertThat(result2, is(new DateTime(LocalDateTime.of(1989, 5, 1, 0, 0))));
		assertThat(result3, is(new DateTime(LocalDateTime.of(2000, 8, 1, 0, 0))));
		assertThat(result4, is(new DateTime(LocalDateTime.of(2010, 2, 1, 0, 0))));
		assertThat(result5, is(new DateTime(LocalDateTime.of(2020, 9, 1, 0, 0))));
		assertThat(result6, is(new DateTime(LocalDateTime.of(2000, 11, 1, 0, 0))));
		assertThat(result7, is(new DateTime(LocalDateTime.of(1990, 12, 1, 0, 0))));
		assertThat(result8, is(new DateTime(LocalDateTime.of(1991, 2, 1, 0, 0))));
		assertThat(result9, is(new DateTime(LocalDateTime.of(1972, 3, 1, 0, 0))));
		assertThat(result10, is(new DateTime(LocalDateTime.of(1892, 1, 1, 0, 0))));
	}
	
	@Test
	public void testRedactTimeOnly() {
		// setup the test data
		TimeOnly input1 = new TimeOnly(LocalTime.of(1, 2, 3, 4));
		TimeOnly input2 = new TimeOnly(LocalTime.of(23, 58, 56, 6));
		TimeOnly input3 = new TimeOnly(LocalTime.of(11, 34, 34, 99));
		TimeOnly input4 = new TimeOnly(LocalTime.of(17, 56, 23, 14));
		TimeOnly input5 = new TimeOnly(LocalTime.of(9, 21, 13, 44));
		TimeOnly input6 = new TimeOnly(LocalTime.of(4, 12, 43, 43));
		TimeOnly input7 = new TimeOnly(LocalTime.of(22, 2, 3, 42));
		TimeOnly input8 = new TimeOnly(LocalTime.of(19, 42, 31, 23));
		TimeOnly input9 = new TimeOnly(LocalTime.of(12, 28, 10, 23));
		TimeOnly input10 = new TimeOnly(LocalTime.of(7, 18, 3, 92));
		
		// call the method under test
		TimeOnly result1 = redactTimeOnly(input1);
		TimeOnly result2 = redactTimeOnly(input2);
		TimeOnly result3 = redactTimeOnly(input3);
		TimeOnly result4 = redactTimeOnly(input4);
		TimeOnly result5 = redactTimeOnly(input5);
		TimeOnly result6 = redactTimeOnly(input6);
		TimeOnly result7 = redactTimeOnly(input7);
		TimeOnly result8 = redactTimeOnly(input8);
		TimeOnly result9 = redactTimeOnly(input9);
		TimeOnly result10 = redactTimeOnly(input10);
		
		// verify the result
		assertThat(result1, is(new TimeOnly(LocalTime.of(1, 0, 0, 0))));
		assertThat(result2, is(new TimeOnly(LocalTime.of(23, 0, 0, 0))));
		assertThat(result3, is(new TimeOnly(LocalTime.of(11, 0, 0, 0))));
		assertThat(result4, is(new TimeOnly(LocalTime.of(17, 0, 0, 0))));
		assertThat(result5, is(new TimeOnly(LocalTime.of(9, 0, 0, 0))));
		assertThat(result6, is(new TimeOnly(LocalTime.of(4, 0, 0, 0))));
		assertThat(result7, is(new TimeOnly(LocalTime.of(22, 0, 0, 0))));
		assertThat(result8, is(new TimeOnly(LocalTime.of(19, 0, 0, 0))));
		assertThat(result9, is(new TimeOnly(LocalTime.of(12, 0, 0, 0))));
		assertThat(result10, is(new TimeOnly(LocalTime.of(7, 0, 0, 0))));
	}
	
	@Test
	public void testRedactTimestamp() {
		// setup the test data
		Timestamp input1 = new Timestamp(LocalDateTime.of(2000, 1, 15, 23, 58));
		Timestamp input2 = new Timestamp(LocalDateTime.of(1989, 5, 11, 21, 45));
		Timestamp input3 = new Timestamp(LocalDateTime.of(2000, 8, 8, 18, 45));
		Timestamp input4 = new Timestamp(LocalDateTime.of(2010, 2, 1, 14, 19));
		Timestamp input5 = new Timestamp(LocalDateTime.of(2020, 9, 18, 11, 59));
		Timestamp input6 = new Timestamp(LocalDateTime.of(2000, 11, 23, 17, 22));
		Timestamp input7 = new Timestamp(LocalDateTime.of(1990, 12, 11, 19, 11));
		Timestamp input8 = new Timestamp(LocalDateTime.of(1991, 2, 12, 23, 36));
		Timestamp input9 = new Timestamp(LocalDateTime.of(1972, 3, 15, 18, 8));
		Timestamp input10 = new Timestamp(LocalDateTime.of(1892, 1, 14, 21, 5));
		
		// call the method under test
		Timestamp result1 = redactTimestamp(input1);
		Timestamp result2 = redactTimestamp(input2);
		Timestamp result3 = redactTimestamp(input3);
		Timestamp result4 = redactTimestamp(input4);
		Timestamp result5 = redactTimestamp(input5);
		Timestamp result6 = redactTimestamp(input6);
		Timestamp result7 = redactTimestamp(input7);
		Timestamp result8 = redactTimestamp(input8);
		Timestamp result9 = redactTimestamp(input9);
		Timestamp result10 = redactTimestamp(input10);
		
		// verify the result
		assertThat(result1, is(new Timestamp(LocalDateTime.of(2000, 1, 1, 0, 0))));
		assertThat(result2, is(new Timestamp(LocalDateTime.of(1989, 5, 1, 0, 0))));
		assertThat(result3, is(new Timestamp(LocalDateTime.of(2000, 8, 1, 0, 0))));
		assertThat(result4, is(new Timestamp(LocalDateTime.of(2010, 2, 1, 0, 0))));
		assertThat(result5, is(new Timestamp(LocalDateTime.of(2020, 9, 1, 0, 0))));
		assertThat(result6, is(new Timestamp(LocalDateTime.of(2000, 11, 1, 0, 0))));
		assertThat(result7, is(new Timestamp(LocalDateTime.of(1990, 12, 1, 0, 0))));
		assertThat(result8, is(new Timestamp(LocalDateTime.of(1991, 2, 1, 0, 0))));
		assertThat(result9, is(new Timestamp(LocalDateTime.of(1972, 3, 1, 0, 0))));
		assertThat(result10, is(new Timestamp(LocalDateTime.of(1892, 1, 1, 0, 0))));
	}
	
	@Test
	public void testRedactGeometry() {
		// setup the test data
		GeometryFactory geometryFactory = new GeometryFactory();
        Coordinate[] pointCoords = new Coordinate[] { new Coordinate(23.12, -91.22) };
        Coordinate[] polylineCoords = new Coordinate[] { new Coordinate(111.12, -78.11), new Coordinate(11.22, -11.1), new Coordinate(-39.1, 221.2) };
        Coordinate[] polygonCoords = new Coordinate[] { new Coordinate(0, 0), new Coordinate(0, 1.12786123), new Coordinate(1.1231231, 1.678969), new Coordinate(1.90678, 0), new Coordinate(0, 0) };
		
        Geometry originalPointGeometry = geometryFactory.createPoint(pointCoords[0]);
        Geometry originalPolylineGeometry = geometryFactory.createLineString(polylineCoords);
        Geometry originalPolygonGeometry = geometryFactory.createPolygon(polygonCoords);
        
		// call the method under test
        Geometry result1 = redactGeometry(originalPointGeometry);
        Geometry result2 = redactGeometry(originalPolylineGeometry);
        Geometry result3 = redactGeometry(originalPolygonGeometry);
        		
		// verify the result
        Coordinate[] modifiedPointCoords = new Coordinate[] { new Coordinate(23, -91) };
		assertThat(result1.getCoordinates(), is(modifiedPointCoords));
		
		Coordinate[] modifiedPolylineCoords = new Coordinate[] { new Coordinate(111, -78), new Coordinate(11, -11), new Coordinate(-39, 221) };
		assertThat(result2.getCoordinates(), is(modifiedPolylineCoords));
		
		Coordinate[] modifiedPolygonCoords = new Coordinate[] { new Coordinate(0, 0), new Coordinate(0, 1), new Coordinate(1, 2), new Coordinate(2, 0), new Coordinate(0, 0) };
		assertThat(result3.getCoordinates(), is(modifiedPolygonCoords));
	}
}
