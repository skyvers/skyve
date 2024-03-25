package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.skyve.impl.backup.BackupUtil.redactDate;
import static org.skyve.impl.backup.BackupUtil.redactGeometry;
import static org.skyve.impl.backup.BackupUtil.redactNumeric;
import static org.skyve.impl.backup.BackupUtil.redactString;
import static org.skyve.impl.backup.BackupUtil.redactTime;
import static org.skyve.impl.backup.BackupUtil.redactTimestamp;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Time;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;

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
		assertThat(result2, is("C**2"));
		assertThat(result3, is("C*3"));
		assertThat(result4, is("C*"));
		assertThat(result5, is("*"));
		assertThat(result6, is(""));
		assertThat(result7, is(nullValue()));
		assertThat(result8, is("Re**********me"));
		assertThat(result9, is("cl***t1@em*****rg"));
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
	public void testRedactBigDecimal() {
		// setup the test data
		BigDecimal input1 = new BigDecimal("0.212342342345254");
		BigDecimal input2 = new BigDecimal("12342.6123234223");
		BigDecimal input3 = new BigDecimal("2345.123234123");
		BigDecimal input4 = new BigDecimal("23141.5523234426423423432");
		BigDecimal input5 = new BigDecimal("-1230.2123423342");
		BigDecimal input6 = new BigDecimal("12340.2123");
		BigDecimal input7 = new BigDecimal("-234234.143543");
		BigDecimal input8 = new BigDecimal("94531.9123121231");
		BigDecimal input9 = new BigDecimal("-0454.012312312");
		BigDecimal input10 = new BigDecimal("8445454.12123131");
		
		// call the method under test
		BigDecimal result1 = redactNumeric(input1);
		BigDecimal result2 = redactNumeric(input2);
		BigDecimal result3 = redactNumeric(input3);
		BigDecimal result4 = redactNumeric(input4);
		BigDecimal result5 = redactNumeric(input5);
		BigDecimal result6 = redactNumeric(input6);
		BigDecimal result7 = redactNumeric(input7);
		BigDecimal result8 = redactNumeric(input8);
		BigDecimal result9 = redactNumeric(input9);
		BigDecimal result10 = redactNumeric(input10);
		
		// verify the result
		assertThat(result1, is(new BigDecimal("0.0")));
		assertThat(result2, is(new BigDecimal("12340.0")));
		assertThat(result3, is(new BigDecimal("2350.0")));
		assertThat(result4, is(new BigDecimal("23140.0")));
		assertThat(result5, is(new BigDecimal("-1230.0")));
		assertThat(result6, is(new BigDecimal("12340.0")));
		assertThat(result7, is(new BigDecimal("-234230.0")));
		assertThat(result8, is(new BigDecimal("94530.0")));
		assertThat(result9, is(new BigDecimal("-0450.0")));
		assertThat(result10, is(new BigDecimal("8445450.0")));
	}
	
	@Test
	public void testRedactDate() {
		// setup the test data
		Date input1 =  Date.valueOf(LocalDate.of(2000, 1, 15));
		Date input2 =  Date.valueOf(LocalDate.of(1989, 6, 11));
		Date input3 =  Date.valueOf(LocalDate.of(2000, 6, 23));
		Date input4 =  Date.valueOf(LocalDate.of(2010, 4, 15));
		Date input5 =  Date.valueOf(LocalDate.of(2020, 3, 2));
		Date input6 =  Date.valueOf(LocalDate.of(2000, 11, 28));
		Date input7 =  Date.valueOf(LocalDate.of(1990, 10, 25));
		Date input8 =  Date.valueOf(LocalDate.of(1991, 10, 19));
		Date input9 =  Date.valueOf(LocalDate.of(1972, 9, 6));
		Date input10 = Date.valueOf(LocalDate.of(1892, 11, 3));
		
		// call the method under test
		Date result1 = redactDate(input1);
		Date result2 = redactDate(input2);
		Date result3 = redactDate(input3);
		Date result4 = redactDate(input4);
		Date result5 = redactDate(input5);
		Date result6 = redactDate(input6);
		Date result7 = redactDate(input7);
		Date result8 = redactDate(input8);
		Date result9 = redactDate(input9);
		Date result10 = redactDate(input10);
		
		// verify the result
		assertThat(result1, is(Date.valueOf(LocalDate.of(2000, 1, 1))));
		assertThat(result2, is(Date.valueOf(LocalDate.of(1989, 6, 1))));
		assertThat(result3, is(Date.valueOf(LocalDate.of(2000, 6, 1))));
		assertThat(result4, is(Date.valueOf(LocalDate.of(2010, 4, 1))));
		assertThat(result5, is(Date.valueOf(LocalDate.of(2020, 3, 1))));
		assertThat(result6, is(Date.valueOf(LocalDate.of(2000, 11, 1))));
		assertThat(result7, is(Date.valueOf(LocalDate.of(1990, 10, 1))));
		assertThat(result8, is(Date.valueOf(LocalDate.of(1991, 10, 1))));
		assertThat(result9, is(Date.valueOf(LocalDate.of(1972, 9, 1))));
		assertThat(result10, is(Date.valueOf(LocalDate.of(1892, 11, 1))));
	}
	
	@Test
	public void testRedactTimestamp() {
		// setup the test data
		java.sql.Timestamp input1 = java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 1, 15, 23, 58));
		java.sql.Timestamp input2 = java.sql.Timestamp.valueOf(LocalDateTime.of(1989, 5, 11, 21, 45));
		java.sql.Timestamp input3 = java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 8, 8, 18, 45));
		java.sql.Timestamp input4 = java.sql.Timestamp.valueOf(LocalDateTime.of(2010, 2, 1, 14, 19));
		java.sql.Timestamp input5 = java.sql.Timestamp.valueOf(LocalDateTime.of(2020, 9, 18, 11, 59));
		java.sql.Timestamp input6 = java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 11, 23, 17, 22));
		java.sql.Timestamp input7 = java.sql.Timestamp.valueOf(LocalDateTime.of(1990, 12, 11, 19, 11));
		java.sql.Timestamp input8 = java.sql.Timestamp.valueOf(LocalDateTime.of(1991, 2, 12, 23, 36));
		java.sql.Timestamp input9 = java.sql.Timestamp.valueOf(LocalDateTime.of(1972, 3, 15, 18, 8));
		java.sql.Timestamp input10 = java.sql.Timestamp.valueOf(LocalDateTime.of(1892, 1, 14, 21, 5));
		
		// call the method under test
		java.sql.Timestamp result1 = redactTimestamp(input1);
		java.sql.Timestamp result2 = redactTimestamp(input2);
		java.sql.Timestamp result3 = redactTimestamp(input3);
		java.sql.Timestamp result4 = redactTimestamp(input4);
		java.sql.Timestamp result5 = redactTimestamp(input5);
		java.sql.Timestamp result6 = redactTimestamp(input6);
		java.sql.Timestamp result7 = redactTimestamp(input7);
		java.sql.Timestamp result8 = redactTimestamp(input8);
		java.sql.Timestamp result9 = redactTimestamp(input9);
		java.sql.Timestamp result10 = redactTimestamp(input10);
		
		// verify the result
		assertThat(result1, is(java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 1, 1, 0, 0))));
		assertThat(result2, is(java.sql.Timestamp.valueOf(LocalDateTime.of(1989, 5, 1, 0, 0))));
		assertThat(result3, is(java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 8, 1, 0, 0))));
		assertThat(result4, is(java.sql.Timestamp.valueOf(LocalDateTime.of(2010, 2, 1, 0, 0))));
		assertThat(result5, is(java.sql.Timestamp.valueOf(LocalDateTime.of(2020, 9, 1, 0, 0))));
		assertThat(result6, is(java.sql.Timestamp.valueOf(LocalDateTime.of(2000, 11, 1, 0, 0))));
		assertThat(result7, is(java.sql.Timestamp.valueOf(LocalDateTime.of(1990, 12, 1, 0, 0))));
		assertThat(result8, is(java.sql.Timestamp.valueOf(LocalDateTime.of(1991, 2, 1, 0, 0))));
		assertThat(result9, is(java.sql.Timestamp.valueOf(LocalDateTime.of(1972, 3, 1, 0, 0))));
		assertThat(result10, is(java.sql.Timestamp.valueOf(LocalDateTime.of(1892, 1, 1, 0, 0))));
	}
	
	@Test
	public void testRedactTime() {
		// setup the test data
		Time input1 = Time.valueOf(LocalTime.of(1, 2, 3, 4));
		Time input2 = Time.valueOf(LocalTime.of(23, 58, 56, 6));
		Time input3 = Time.valueOf(LocalTime.of(11, 34, 34, 99));
		Time input4 = Time.valueOf(LocalTime.of(17, 56, 23, 14));
		Time input5 = Time.valueOf(LocalTime.of(9, 21, 13, 44));
		Time input6 = Time.valueOf(LocalTime.of(4, 12, 43, 43));
		Time input7 = Time.valueOf(LocalTime.of(22, 2, 3, 42));
		Time input8 = Time.valueOf(LocalTime.of(19, 42, 31, 23));
		Time input9 = Time.valueOf(LocalTime.of(12, 28, 10, 23));
		Time input10 = Time.valueOf(LocalTime.of(7, 18, 3, 92));
		
		// call the method under test
		Time result1 = redactTime(input1);
		Time result2 = redactTime(input2);
		Time result3 = redactTime(input3);
		Time result4 = redactTime(input4);
		Time result5 = redactTime(input5);
		Time result6 = redactTime(input6);
		Time result7 = redactTime(input7);
		Time result8 = redactTime(input8);
		Time result9 = redactTime(input9);
		Time result10 = redactTime(input10);
		
		// verify the result
		assertThat(result1, is(Time.valueOf(LocalTime.of(1, 0, 0, 0))));
		assertThat(result2, is(Time.valueOf(LocalTime.of(23, 0, 0, 0))));
		assertThat(result3, is(Time.valueOf(LocalTime.of(11, 0, 0, 0))));
		assertThat(result4, is(Time.valueOf(LocalTime.of(17, 0, 0, 0))));
		assertThat(result5, is(Time.valueOf(LocalTime.of(9, 0, 0, 0))));
		assertThat(result6, is(Time.valueOf(LocalTime.of(4, 0, 0, 0))));
		assertThat(result7, is(Time.valueOf(LocalTime.of(22, 0, 0, 0))));
		assertThat(result8, is(Time.valueOf(LocalTime.of(19, 0, 0, 0))));
		assertThat(result9, is(Time.valueOf(LocalTime.of(12, 0, 0, 0))));
		assertThat(result10, is(Time.valueOf(LocalTime.of(7, 0, 0, 0))));
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
