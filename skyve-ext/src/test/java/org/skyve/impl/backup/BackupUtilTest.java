package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.skyve.impl.backup.BackupUtil.redactData;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.customer.ExportedReference;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;
import org.skyve.metadata.user.User;

public class BackupUtilTest {
	@Test
	@SuppressWarnings("static-method")
	public void testRedactSegment() {
		// setup the test data
		String input1 = "Client 1",
				input2 = "Cli2",
				input3 = "Cl3",
				input4 = "C4",
				input5 = "C",
				input6 = "",
				input7 = null,
				input8 = "Really long client name",
				input9 = "client1@email.org",
				input10 = "Cli2",
				input11 = "Cl3",
				input12 = "C4",
				input13 = "C";

		// call the method under test
		String result1 = (String) redactData(AttributeType.text, input1);
		String result2 = (String) redactData(AttributeType.markup, input2);
		String result3 = (String) redactData(AttributeType.memo, input3);
		String result4 = (String) redactData(AttributeType.id, input4);
		String result5 = (String) redactData(AttributeType.text, input5);
		String result6 = (String) redactData(AttributeType.markup, input6);
		String result7 = (String) redactData(AttributeType.memo, input7);
		String result8 = (String) redactData(AttributeType.id, input8);
		String result9 = (String) redactData(AttributeType.text, input9);
		String result10 = (String) redactData(AttributeType.text, input10, Integer.valueOf(4));
		String result11 = (String) redactData(AttributeType.text, input11, Integer.valueOf(4));
		String result12 = (String) redactData(AttributeType.text, input12, Integer.valueOf(3));
		String result13 = (String) redactData(AttributeType.text, input13, Integer.valueOf(2));
		
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
		assertThat(result10, is("C**2"));
		assertThat(result11, is("C**3"));
		assertThat(result12, is("C**"));
		assertThat(result13, is("**"));
	}
	
	@Test
	@SuppressWarnings("static-method")
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
		Integer result1 = (Integer) redactData(AttributeType.integer, input1);
		Integer result2 = (Integer) redactData(AttributeType.integer, input2);
		Integer result3 = (Integer) redactData(AttributeType.integer, input3);
		Integer result4 = (Integer) redactData(AttributeType.integer, input4);
		Integer result5 = (Integer) redactData(AttributeType.integer, input5);
		Integer result6 = (Integer) redactData(AttributeType.integer, input6);
		Integer result7 = (Integer) redactData(AttributeType.integer, input7);
		Integer result8 = (Integer) redactData(AttributeType.integer, input8);
		Integer result9 = (Integer) redactData(AttributeType.integer, input9);
		Integer result10 = (Integer) redactData(AttributeType.integer, input10);
		
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
	@SuppressWarnings("static-method")
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
		Long result1 = (Long) redactData(AttributeType.longInteger, input1);
		Long result2 = (Long) redactData(AttributeType.longInteger, input2);
		Long result3 = (Long) redactData(AttributeType.longInteger, input3);
		Long result4 = (Long) redactData(AttributeType.longInteger, input4);
		Long result5 = (Long) redactData(AttributeType.longInteger, input5);
		Long result6 = (Long) redactData(AttributeType.longInteger, input6);
		Long result7 = (Long) redactData(AttributeType.longInteger, input7);
		Long result8 = (Long) redactData(AttributeType.longInteger, input8);
		Long result9 = (Long) redactData(AttributeType.longInteger, input9);
		Long result10 = (Long) redactData(AttributeType.longInteger, input10);
		
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
	@SuppressWarnings("static-method")
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
		BigDecimal result1 = (BigDecimal) redactData(AttributeType.decimal2, input1);
		BigDecimal result2 = (BigDecimal) redactData(AttributeType.decimal5, input2);
		BigDecimal result3 = (BigDecimal) redactData(AttributeType.decimal10, input3);
		BigDecimal result4 = (BigDecimal) redactData(AttributeType.decimal2, input4);
		BigDecimal result5 = (BigDecimal) redactData(AttributeType.decimal5, input5);
		BigDecimal result6 = (BigDecimal) redactData(AttributeType.decimal10, input6);
		BigDecimal result7 = (BigDecimal) redactData(AttributeType.decimal2, input7);
		BigDecimal result8 = (BigDecimal) redactData(AttributeType.decimal5, input8);
		BigDecimal result9 = (BigDecimal) redactData(AttributeType.decimal10, input9);
		BigDecimal result10 = (BigDecimal) redactData(AttributeType.decimal2, input10);
		
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
	@SuppressWarnings("static-method")
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
		Date result1 = (Date) redactData(AttributeType.date, input1);
		Date result2 = (Date) redactData(AttributeType.date, input2);
		Date result3 = (Date) redactData(AttributeType.date, input3);
		Date result4 = (Date) redactData(AttributeType.date, input4);
		Date result5 = (Date) redactData(AttributeType.date, input5);
		Date result6 = (Date) redactData(AttributeType.date, input6);
		Date result7 = (Date) redactData(AttributeType.date, input7);
		Date result8 = (Date) redactData(AttributeType.date, input8);
		Date result9 = (Date) redactData(AttributeType.date, input9);
		Date result10 = (Date) redactData(AttributeType.date, input10);
		
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
	@SuppressWarnings("static-method")
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
		java.sql.Timestamp result1 = (Timestamp) redactData(AttributeType.dateTime, input1);
		java.sql.Timestamp result2 = (Timestamp) redactData(AttributeType.timestamp, input2);
		java.sql.Timestamp result3 = (Timestamp) redactData(AttributeType.dateTime, input3);
		java.sql.Timestamp result4 = (Timestamp) redactData(AttributeType.timestamp, input4);
		java.sql.Timestamp result5 = (Timestamp) redactData(AttributeType.dateTime, input5);
		java.sql.Timestamp result6 = (Timestamp) redactData(AttributeType.timestamp, input6);
		java.sql.Timestamp result7 = (Timestamp) redactData(AttributeType.dateTime, input7);
		java.sql.Timestamp result8 = (Timestamp) redactData(AttributeType.timestamp, input8);
		java.sql.Timestamp result9 = (Timestamp) redactData(AttributeType.dateTime, input9);
		java.sql.Timestamp result10 = (Timestamp) redactData(AttributeType.timestamp, input10);
		
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
	@SuppressWarnings("static-method")
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
		Time result1 = (Time) redactData(AttributeType.time, input1);
		Time result2 = (Time) redactData(AttributeType.time, input2);
		Time result3 = (Time) redactData(AttributeType.time, input3);
		Time result4 = (Time) redactData(AttributeType.time, input4);
		Time result5 = (Time) redactData(AttributeType.time, input5);
		Time result6 = (Time) redactData(AttributeType.time, input6);
		Time result7 = (Time) redactData(AttributeType.time, input7);
		Time result8 = (Time) redactData(AttributeType.time, input8);
		Time result9 = (Time) redactData(AttributeType.time, input9);
		Time result10 = (Time) redactData(AttributeType.time, input10);
		
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
	@SuppressWarnings("static-method")
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
        Geometry result1 = (Geometry) redactData(AttributeType.geometry, originalPointGeometry);
        Geometry result2 = (Geometry) redactData(AttributeType.geometry, originalPolylineGeometry);
        Geometry result3 = (Geometry) redactData(AttributeType.geometry, originalPolygonGeometry);
        		
		// verify the result
        Coordinate[] modifiedPointCoords = new Coordinate[] { new Coordinate(23, -91) };
		assertThat(result1.getCoordinates(), is(modifiedPointCoords));
		
		Coordinate[] modifiedPolylineCoords = new Coordinate[] { new Coordinate(111, -78), new Coordinate(11, -11), new Coordinate(-39, 221) };
		assertThat(result2.getCoordinates(), is(modifiedPolylineCoords));
		
		Coordinate[] modifiedPolygonCoords = new Coordinate[] { new Coordinate(0, 0), new Coordinate(0, 1), new Coordinate(1, 2), new Coordinate(2, 0), new Coordinate(0, 0) };
		assertThat(result3.getCoordinates(), is(modifiedPolygonCoords));
	}

	// ---- hasBizCustomer tests ----

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerReturnsFalseForTableWithoutCustomerField() {
		Table table = new Table("myTable", "myTable");
		table.fields.put("bizId", Table.TEXT);
		table.fields.put("bizKey", Table.TEXT);
		assertFalse(BackupUtil.hasBizCustomer(table));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerReturnsTrueWhenCustomerFieldPresent() {
		Table table = new Table("myTable", "myTable");
		table.fields.put("bizId", Table.TEXT);
		table.fields.put(Bean.CUSTOMER_NAME, Table.TEXT);
		assertTrue(BackupUtil.hasBizCustomer(table));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerReturnsTrueForMixedCaseFieldName() {
		Table table = new Table("myTable", "myTable");
		table.fields.put("BIZCUSTOMER", Table.TEXT);  // CUSTOMER_NAME = "bizCustomer"
		// equalsIgnoreCase so "BIZCUSTOMER" matches "bizCustomer"
		assertTrue(BackupUtil.hasBizCustomer(table));
	}

	// ---- secureSQL tests ----

	@Test
	@SuppressWarnings("static-method")
	public void secureSQLForJoinTableAlwaysAddsOwnerConstraint() {
		JoinTable joinTable = new JoinTable("owner_roles", "owner_roles", "Owner", "Owner", false);
		StringBuilder sql = new StringBuilder("select * from owner_roles");
		BackupUtil.secureSQL(sql, joinTable, "testCustomer");
		String result = sql.toString();
		assertThat(result, containsString("where"));
		assertThat(result, containsString("owner_id"));
		assertThat(result, containsString("Owner"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void secureSQLForRegularTableWithNoCustomerFieldDoesNotAppend() {
		Table table = new Table("NoCustomerTable", "NoCustomerTable");
		table.fields.put("bizId", Table.TEXT);
		StringBuilder sql = new StringBuilder("select * from NoCustomerTable");
		BackupUtil.secureSQL(sql, table, "testCustomer");
		assertFalse("SQL should remain non-empty after secureSQL", sql.isEmpty());
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataReturnsNullForNullValue() {
		assertThat(redactData(AttributeType.text, null), is(nullValue()));
		assertThat(redactData(AttributeType.integer, null), is(nullValue()));
		assertThat(redactData(AttributeType.date, null), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataReturnsValueUnchangedForUnknownType() {
		// association type is not redacted by default — value returned unchanged
		String value = "some-association-id";
		Object result = redactData(AttributeType.association, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataContentTypeReturnsNull() {
		// content and image fields are nullified
		Object result = redactData(AttributeType.content, "some-content-id");
		assertThat(result, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataImageTypeReturnsNull() {
		Object result = redactData(AttributeType.image, "image-id");
		assertThat(result, is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataDateTypeRoundsToFirstOfMonth() {
		Date input = Date.valueOf(LocalDate.of(2023, Month.JULY, 15));
		Date result = (Date) redactData(AttributeType.date, input);
		assertThat(result, is(Date.valueOf(LocalDate.of(2023, Month.JULY, 1))));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataTimeTypeRoundsToHour() {
		Time input = Time.valueOf(LocalTime.of(14, 37, 22));
		Time result = (Time) redactData(AttributeType.time, input);
		assertThat(result, is(Time.valueOf(LocalTime.of(14, 0, 0))));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataDateTimeTypeRoundsToFirstOfMonth() {
		Timestamp input = Timestamp.valueOf(LocalDateTime.of(2023, Month.AUGUST, 20, 10, 30, 0));
		Timestamp result = (Timestamp) redactData(AttributeType.dateTime, input);
		assertThat(result, is(Timestamp.valueOf(LocalDateTime.of(2023, Month.AUGUST, 1, 0, 0, 0))));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataTimestampTypeRoundsToFirstOfMonth() {
		Timestamp input = Timestamp.valueOf(LocalDateTime.of(2021, Month.MARCH, 14, 9, 15, 0));
		Timestamp result = (Timestamp) redactData(AttributeType.timestamp, input);
		assertThat(result, is(Timestamp.valueOf(LocalDateTime.of(2021, Month.MARCH, 1, 0, 0, 0))));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataDecimal2TypeRoundsToNearestTen() {
		BigDecimal input = BigDecimal.valueOf(47.5);
		BigDecimal result = (BigDecimal) redactData(AttributeType.decimal2, input);
		assertThat(result, is(BigDecimal.valueOf(50.0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataDecimal5TypeRoundsToNearestTen() {
		BigDecimal input = BigDecimal.valueOf(23.12345);
		BigDecimal result = (BigDecimal) redactData(AttributeType.decimal5, input);
		assertThat(result, is(BigDecimal.valueOf(20.0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataDecimal10TypeRoundsToNearestTen() {
		BigDecimal input = BigDecimal.valueOf(99.9999);
		BigDecimal result = (BigDecimal) redactData(AttributeType.decimal10, input);
		assertThat(result, is(BigDecimal.valueOf(100.0)));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataBoolTypeReturnsSameBooleanValue() {
		Object result = redactData(AttributeType.bool, Boolean.TRUE);
		assertThat(result, is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataEnumerationTypeReturnsValueUnchanged() {
		String value = "ACTIVE";
		Object result = redactData(AttributeType.enumeration, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataColourTypeReturnsValueUnchanged() {
		String value = "#FF0000";
		Object result = redactData(AttributeType.colour, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void writeAndReadScriptRoundtripsCommands() throws Exception {
		List<String> commands = Arrays.asList("INSERT INTO foo VALUES (1)", "UPDATE bar SET x = 2");
		File tempFile = Files.createTempFile("backup_test_", ".sql").toFile();
		try {
			BackupUtil.writeScript(commands, tempFile);
			List<String> result = BackupUtil.readScript(tempFile);
			assertEquals(2, result.size());
			assertThat(result.get(0), is("INSERT INTO foo VALUES (1)"));
			assertThat(result.get(1), is("UPDATE bar SET x = 2"));
		}
		finally {
			Files.delete(tempFile.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void readScriptStripsTrailingSemicolon() throws Exception {
		List<String> commands = Arrays.asList("SELECT 1");
		File tempFile = Files.createTempFile("backup_test_", ".sql").toFile();
		try {
			BackupUtil.writeScript(commands, tempFile);
			List<String> result = BackupUtil.readScript(tempFile);
			// writeScript adds ; at end, readScript strips it
			assertThat(result.get(0), is("SELECT 1"));
		}
		finally {
			Files.delete(tempFile.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerReturnsFalseForTableWithNoCustomerField() {
		Table table = new Table("test", "test");
		assertFalse(BackupUtil.hasBizCustomer(table));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerReturnsTrueForTableWithCustomerField() {
		Table table = new Table("test", "test");
		table.fields.put(Bean.CUSTOMER_NAME, Table.TEXT);
		assertTrue(BackupUtil.hasBizCustomer(table));
	}

	@Test
	@SuppressWarnings("static-method")
	public void hasBizCustomerIsCaseInsensitive() {
		Table table = new Table("test", "test");
		table.fields.put(Bean.CUSTOMER_NAME.toUpperCase(), Table.TEXT);
		assertTrue(BackupUtil.hasBizCustomer(table));
	}

	@Test
	@SuppressWarnings("static-method")
	public void secureSQLAppendsNothingForSingleTenantTableWithoutCustomerField() {
		String previous = UtilImpl.CUSTOMER;
		UtilImpl.CUSTOMER = "acme";
		try {
			Table table = new Table("test", "test");
			StringBuilder sql = new StringBuilder("SELECT * FROM test");
			BackupUtil.secureSQL(sql, table, "acme");
			assertThat(sql.toString(), is("SELECT * FROM test"));
		}
		finally {
			UtilImpl.CUSTOMER = previous;
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void tableToJSONRoundtripsViaFromJSON() throws Exception {
		Table table = new Table("myTable", "myTable");
		table.fields.put("id", Table.TEXT);
		table.fields.put("amount", Table.INTEGER);

		String json = table.toJSON();
		assertThat(json, containsString("myTable"));

		Table restored = Table.fromJSON(json);
		assertThat(restored.agnosticIdentifier, is("myTable"));
		assertTrue(restored.fields.containsKey("id"));
		assertTrue(restored.fields.containsKey("amount"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void tableEqualsBasedOnAgnosticIdentifier() {
		Table t1 = new Table("same", "same");
		Table t2 = new Table("same", "different");
		Table t3 = new Table("other", "same");
		assertEquals(t1, t2);
		assertNotEquals(t1, t3);
	}

	@Test
	@SuppressWarnings("static-method")
	public void tableHashCodeConsistentWithEquals() {
		Table t1 = new Table("abc", "abc");
		Table t2 = new Table("abc", "xyz");
		assertEquals(t2.hashCode(), t1.hashCode());
	}

	@Test
	@SuppressWarnings("static-method")
	public void writeAndReadTablesRoundtrips() throws Exception {
		Table t1 = new Table("table1", "Table1");
		t1.fields.put("bizId", Table.TEXT);
		t1.fields.put("bizVersion", Table.INTEGER);
		Table t2 = new Table("table2", "Table2");
		t2.fields.put("name", Table.TEXT);
		java.util.List<Table> tables = Arrays.asList(t1, t2);

		File tempFile = Files.createTempFile("backup_tables_", ".json").toFile();
		try {
			BackupUtil.writeTables(tables, tempFile);
			java.util.Collection<Table> result = BackupUtil.readTables(tempFile);
			assertEquals(2, result.size());
		}
		finally {
			Files.delete(tempFile.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void writeAndReadTablesPreservesFieldTypes() throws Exception {
		Table t = new Table("myTable", "myTable");
		t.fields.put("textField", Table.TEXT);
		t.fields.put("intField", Table.INTEGER);
		t.fields.put("assocField", Table.ASSOCIATION);
		java.util.List<Table> tables = Arrays.asList(t);

		File tempFile = Files.createTempFile("backup_tables_fields_", ".json").toFile();
		try {
			BackupUtil.writeTables(tables, tempFile);
			java.util.Collection<Table> result = BackupUtil.readTables(tempFile);
			Table restored = result.iterator().next();
			assertThat(restored.agnosticIdentifier, is("myTable"));
			assertTrue(restored.fields.containsKey("textField"));
			assertTrue(restored.fields.containsKey("intField"));
			assertTrue(restored.fields.containsKey("assocField"));
		}
		finally {
			Files.delete(tempFile.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataNullValueReturnsNull() {
		Object result = redactData(AttributeType.text, null);
		assertThat(result, nullValue());
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataCollectionTypeReturnsValueUnchanged() {
		String value = "col-value";
		Object result = redactData(AttributeType.collection, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataInverseManyTypeReturnsValueUnchanged() {
		String value = "inv-many-value";
		Object result = redactData(AttributeType.inverseMany, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void redactDataInverseOneTypeReturnsValueUnchanged() {
		String value = "inv-one-value";
		Object result = redactData(AttributeType.inverseOne, value);
		assertThat(result, is(value));
	}

	@Test
	@SuppressWarnings("static-method")
	public void joinTableToJSONAndFromJSONRoundtrips() throws Exception {
		JoinTable joinTable = new JoinTable("ownerTable_elements", "ownerTable_elements",
				"ownerTable", "ownerTable", false);
		String json = joinTable.toJSON();
		assertThat(json, containsString("ownerTable"));
		Table restored = Table.fromJSON(json);
		assertTrue(restored instanceof JoinTable);
		JoinTable restoredJoin = (JoinTable) restored;
		assertThat(restoredJoin.agnosticIdentifier, is("ownerTable_elements"));
		assertThat(restoredJoin.ownerAgnosticIdentifier, is("ownerTable"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderedJoinTableToJSONAndFromJSONRoundtrips() throws Exception {
		JoinTable joinTable = new JoinTable("list_items", "list_items",
				"myList", "myList", true);
		String json = joinTable.toJSON();
		Table restored = Table.fromJSON(json);
		assertTrue(restored instanceof JoinTable);
		JoinTable restoredJoin = (JoinTable) restored;
		assertTrue(restoredJoin.ordered);
	}

	@Test
	@SuppressWarnings("static-method")
	public void joinTableHasOwnerAndElementFields() {
		JoinTable joinTable = new JoinTable("t_elem", "t_elem", "t_owner", "t_owner", false);
		assertTrue(joinTable.fields.containsKey("owner_id"));
		assertTrue(joinTable.fields.containsKey("element_id"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void orderedJoinTableHasOrderByField() {
		JoinTable joinTable = new JoinTable("t_elem", "t_elem", "t_owner", "t_owner", true);
		assertTrue(joinTable.fields.containsKey("bizOrdinal"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void tableFromJSONPreservesFieldTypes() throws Exception {
		Table t = new Table("myTable", "myTable");
		t.fields.put("bizId", Table.TEXT);
		t.fields.put("amount", Table.INTEGER);
		t.fields.put("owner", Table.ASSOCIATION);
		String json = t.toJSON();
		Table restored = Table.fromJSON(json);
		assertThat(restored.fields.get("bizId").getAttributeType(), is(AttributeType.text));
		assertThat(restored.fields.get("amount").getAttributeType(), is(AttributeType.integer));
		assertThat(restored.fields.get("owner").getAttributeType(), is(AttributeType.association));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void getTablesReturnsSingleTableForSimpleStaticDocument() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentRef documentRef = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Persistent persistent = mock(Persistent.class);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("MyDoc", documentRef);

		org.skyve.metadata.model.Attribute.Sensitivity sensitivity = org.skyve.metadata.model.Attribute.Sensitivity.none;

		org.skyve.metadata.model.Extends extension = null;

		try {
			when(persistence.getUser()).thenReturn(user);
			when(user.getCustomer()).thenReturn(customer);
			when(customer.getModules()).thenReturn(Collections.singletonList(module));
			when(customer.getModule("mod")).thenReturn(module);
			when(customer.getExportedReferences(document)).thenReturn(null);
			when(module.getName()).thenReturn("mod");
			when(module.getDocumentRefs()).thenReturn(refs);
			when(module.getDocument(customer, "MyDoc")).thenReturn(document);
			when(documentRef.getOwningModuleName()).thenReturn("mod");
			when(document.isDynamic()).thenReturn(false);
			when(document.isPersistable()).thenReturn(true);
			when(document.getPersistent()).thenReturn(persistent);
			when(document.getOwningModuleName()).thenReturn("mod");
			when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
			when(document.getParentDocumentName()).thenReturn(null);
			when(document.isOrdered()).thenReturn(false);
			when(document.getBizKeySensitity()).thenReturn(sensitivity);
			when(document.getExtends()).thenReturn(extension);
			when(persistent.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistent.getPersistentIdentifier()).thenReturn("T_DOC");
			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document)).thenReturn(document);

			withThreadLocalPersistence(persistence, () -> {
				Collection<Table> tables = BackupUtil.getTables();
				assertEquals(1, tables.size());
				Table table = tables.iterator().next();
				assertEquals("T_DOC", table.agnosticIdentifier);
				assertTrue(table.fields.containsKey(Bean.DOCUMENT_ID));
				assertTrue(table.fields.containsKey(Bean.BIZ_KEY));
			});
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void getTablesAddsJoinTableForAggregationReference() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentRef documentRef = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Persistent persistent = mock(Persistent.class);

		Document referencedDocument = mock(Document.class);
		org.skyve.metadata.model.document.Collection collection = mock(org.skyve.metadata.model.document.Collection.class);
		Persistent ownerPersistent = mock(Persistent.class);

		ExportedReference reference = new ExportedReference();
		reference.setModuleName("mod");
		reference.setDocumentName("RefDoc");
		reference.setReferenceFieldName("items");
		reference.setType(CollectionType.aggregation);
		reference.setPersistent(ownerPersistent);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("MyDoc", documentRef);

		org.skyve.metadata.model.Attribute.Sensitivity sensitivity = org.skyve.metadata.model.Attribute.Sensitivity.none;

		try {
			when(persistence.getUser()).thenReturn(user);
			when(user.getCustomer()).thenReturn(customer);
			when(customer.getModules()).thenReturn(Collections.singletonList(module));
			when(customer.getModule("mod")).thenReturn(module);
			when(customer.getExportedReferences(document)).thenReturn(Collections.singletonList(reference));
			when(module.getName()).thenReturn("mod");
			when(module.getDocumentRefs()).thenReturn(refs);
			when(module.getDocument(customer, "MyDoc")).thenReturn(document);
			when(module.getDocument(customer, "RefDoc")).thenReturn(referencedDocument);
			when(documentRef.getOwningModuleName()).thenReturn("mod");
			when(document.isDynamic()).thenReturn(false);
			when(document.isPersistable()).thenReturn(true);
			when(document.getPersistent()).thenReturn(persistent);
			when(document.getOwningModuleName()).thenReturn("mod");
			when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
			when(document.getParentDocumentName()).thenReturn(null);
			when(document.isOrdered()).thenReturn(false);
			when(document.getBizKeySensitity()).thenReturn(sensitivity);
			when(document.getExtends()).thenReturn(null);
			when(persistent.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistent.getPersistentIdentifier()).thenReturn("T_DOC");
			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document)).thenReturn(document);

			when(referencedDocument.isDynamic()).thenReturn(false);
			when(referencedDocument.getReferenceByName("items")).thenReturn(collection);
			when(collection.isPersistent()).thenReturn(true);
			when(collection.getOrdered()).thenReturn(Boolean.TRUE);

			when(ownerPersistent.getAgnosticIdentifier()).thenReturn("T_OWNER");
			when(ownerPersistent.getPersistentIdentifier()).thenReturn("T_OWNER");
			when(ownerPersistent.getStrategy()).thenReturn(null);
			when(ownerPersistent.isPolymorphicallyMapped()).thenReturn(false);

			withThreadLocalPersistence(persistence, () -> {
				Collection<Table> tables = BackupUtil.getTables();
				assertEquals(2, tables.size());
			});
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void getTablesAddsJoinTableForJoinedReferenceStrategy() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentRef documentRef = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Persistent persistent = mock(Persistent.class);

		Document referencedDocument = mock(Document.class);
		org.skyve.metadata.model.document.Collection collection = mock(org.skyve.metadata.model.document.Collection.class);
		Persistent referencePersistent = mock(Persistent.class);
		Persistent referencedPersistent = mock(Persistent.class);

		ExportedReference reference = new ExportedReference();
		reference.setModuleName("mod");
		reference.setDocumentName("RefDoc");
		reference.setReferenceFieldName("items");
		reference.setType(CollectionType.aggregation);
		reference.setPersistent(referencePersistent);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("MyDoc", documentRef);

		org.skyve.metadata.model.Attribute.Sensitivity sensitivity = org.skyve.metadata.model.Attribute.Sensitivity.none;

		try {
			when(persistence.getUser()).thenReturn(user);
			when(user.getCustomer()).thenReturn(customer);
			when(customer.getModules()).thenReturn(Collections.singletonList(module));
			when(customer.getModule("mod")).thenReturn(module);
			when(customer.getExportedReferences(document)).thenReturn(Collections.singletonList(reference));
			when(module.getName()).thenReturn("mod");
			when(module.getDocumentRefs()).thenReturn(refs);
			when(module.getDocument(customer, "MyDoc")).thenReturn(document);
			when(module.getDocument(customer, "RefDoc")).thenReturn(referencedDocument);
			when(documentRef.getOwningModuleName()).thenReturn("mod");
			when(document.isDynamic()).thenReturn(false);
			when(document.isPersistable()).thenReturn(true);
			when(document.getPersistent()).thenReturn(persistent);
			when(document.getOwningModuleName()).thenReturn("mod");
			when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
			when(document.getParentDocumentName()).thenReturn(null);
			when(document.isOrdered()).thenReturn(false);
			when(document.getBizKeySensitity()).thenReturn(sensitivity);
			when(document.getExtends()).thenReturn(null);
			when(persistent.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistent.getPersistentIdentifier()).thenReturn("T_DOC");
			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document)).thenReturn(document);

			when(referencedDocument.isDynamic()).thenReturn(false);
			when(referencedDocument.getReferenceByName("items")).thenReturn(collection);
			when(referencedDocument.getExtends()).thenReturn(null);
			when(referencedDocument.getPersistent()).thenReturn(referencedPersistent);
			when(collection.isPersistent()).thenReturn(true);
			when(collection.getOrdered()).thenReturn(Boolean.FALSE);

			when(referencePersistent.getAgnosticIdentifier()).thenReturn("T_OWNER");
			when(referencePersistent.getPersistentIdentifier()).thenReturn("T_OWNER");
			when(referencePersistent.getStrategy()).thenReturn(org.skyve.metadata.model.Persistent.ExtensionStrategy.joined);
			when(referencePersistent.isPolymorphicallyMapped()).thenReturn(false);
			when(referencedPersistent.getAgnosticIdentifier()).thenReturn("T_REF");
			when(referencedPersistent.getPersistentIdentifier()).thenReturn("T_REF");

			withThreadLocalPersistence(persistence, () -> {
				Collection<Table> tables = BackupUtil.getTables();
				assertEquals(2, tables.size());
			});
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void getTablesForAllCustomersThrowsWhenCustomerMissing() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		try {
			ProvidedRepositoryFactory.set(repository);
			when(repository.getAllCustomerNames()).thenReturn(Collections.singletonList("missingCustomer"));
			when(repository.getCustomer("missingCustomer")).thenReturn(null);

			try {
				BackupUtil.getTablesForAllCustomers();
				org.junit.Assert.fail("Expected MetaDataException");
			}
			catch (MetaDataException e) {
				assertThat(e.getMessage(), containsString("missingCustomer does not exist"));
			}
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void getTablesForAllCustomersReturnsMergedTables() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		try {
			ProvidedRepositoryFactory.set(repository);

			CustomerImpl customerOne = mock(CustomerImpl.class);
			CustomerImpl customerTwo = mock(CustomerImpl.class);
			Module moduleOne = mock(Module.class);
			Module moduleTwo = mock(Module.class);
			DocumentRef refOne = mock(DocumentRef.class);
			DocumentRef refTwo = mock(DocumentRef.class);
			Document docOne = mock(Document.class);
			Document docTwo = mock(Document.class);
			Persistent persistentOne = mock(Persistent.class);
			Persistent persistentTwo = mock(Persistent.class);

			Map<String, DocumentRef> refsOne = new HashMap<>();
			refsOne.put("MyDoc", refOne);
			Map<String, DocumentRef> refsTwo = new HashMap<>();
			refsTwo.put("MyDoc", refTwo);

			org.skyve.metadata.model.Attribute.Sensitivity sensitivity = org.skyve.metadata.model.Attribute.Sensitivity.none;

			when(repository.getAllCustomerNames()).thenReturn(Arrays.asList("c1", "c2"));
			when(repository.getCustomer("c1")).thenReturn(customerOne);
			when(repository.getCustomer("c2")).thenReturn(customerTwo);

			when(customerOne.getModules()).thenReturn(Collections.singletonList(moduleOne));
			when(customerTwo.getModules()).thenReturn(Collections.singletonList(moduleTwo));

			when(moduleOne.getName()).thenReturn("mod");
			when(moduleTwo.getName()).thenReturn("mod");
			when(moduleOne.getDocumentRefs()).thenReturn(refsOne);
			when(moduleTwo.getDocumentRefs()).thenReturn(refsTwo);
			when(refOne.getOwningModuleName()).thenReturn("mod");
			when(refTwo.getOwningModuleName()).thenReturn("mod");

			when(moduleOne.getDocument(customerOne, "MyDoc")).thenReturn(docOne);
			when(moduleTwo.getDocument(customerTwo, "MyDoc")).thenReturn(docTwo);

			when(docOne.isDynamic()).thenReturn(false);
			when(docOne.isPersistable()).thenReturn(true);
			when(docOne.getPersistent()).thenReturn(persistentOne);
			when(docOne.getOwningModuleName()).thenReturn("mod");
			when(docOne.getAllAttributes(customerOne)).thenReturn(Collections.emptyList());
			when(docOne.getParentDocumentName()).thenReturn(null);
			when(docOne.isOrdered()).thenReturn(false);
			when(docOne.getBizKeySensitity()).thenReturn(sensitivity);
			when(docOne.getExtends()).thenReturn(null);
			when(customerOne.getExportedReferences(docOne)).thenReturn(Collections.emptyList());

			when(docTwo.isDynamic()).thenReturn(false);
			when(docTwo.isPersistable()).thenReturn(true);
			when(docTwo.getPersistent()).thenReturn(persistentTwo);
			when(docTwo.getOwningModuleName()).thenReturn("mod");
			when(docTwo.getAllAttributes(customerTwo)).thenReturn(Collections.emptyList());
			when(docTwo.getParentDocumentName()).thenReturn(null);
			when(docTwo.isOrdered()).thenReturn(false);
			when(docTwo.getBizKeySensitity()).thenReturn(sensitivity);
			when(docTwo.getExtends()).thenReturn(null);
			when(customerTwo.getExportedReferences(docTwo)).thenReturn(Collections.emptyList());

			when(persistentOne.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistentOne.getPersistentIdentifier()).thenReturn("T_DOC");
			when(persistentTwo.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistentTwo.getPersistentIdentifier()).thenReturn("T_DOC");

			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customerOne, moduleOne, docOne)).thenReturn(docOne);
			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customerTwo, moduleTwo, docTwo)).thenReturn(docTwo);

			Collection<Table> tables = BackupUtil.getTablesForAllCustomers();
			assertEquals(1, tables.size());
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void validateSkyveBackupThrowsWhenDirectoryMissing() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomerName()).thenReturn("missingDirCustomer");

		withThreadLocalPersistence(persistence, () -> {
			String extractName = "missing-backup-dir-" + System.nanoTime();
			try {
				BackupUtil.validateSkyveBackup(extractName);
			}
			catch (DomainException e) {
				assertThat(e.getMessage(), containsString("is not a directory"));
				return;
			}
			org.junit.Assert.fail("Expected DomainException");
		});
	}

	@Test
	@SuppressWarnings("static-method")
	public void validateSkyveBackupThrowsWhenRootContainsNoCsvFiles() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomerName()).thenReturn("noCsvCustomer");
		String previousBackupDirectory = UtilImpl.BACKUP_DIRECTORY;
		File root = Files.createTempDirectory("skyve-backup-util-").toFile();
		String extractName = "extract";
		File backupDirectory = new File(new File(root, "backup_noCsvCustomer"), extractName);
		assertTrue(backupDirectory.mkdirs());
		UtilImpl.BACKUP_DIRECTORY = root.getAbsolutePath() + File.separator;

		try {
			withThreadLocalPersistence(persistence, () -> {
				try {
					BackupUtil.validateSkyveBackup(extractName);
				}
				catch (DomainException e) {
					assertThat(e.getMessage(), containsString("No valid Skyve CSV files"));
					return;
				}
				org.junit.Assert.fail("Expected DomainException");
			});
		}
		finally {
			UtilImpl.BACKUP_DIRECTORY = previousBackupDirectory;
			Files.delete(backupDirectory.toPath());
			Files.delete(backupDirectory.getParentFile().toPath());
			Files.delete(root.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void validateSkyveBackupReturnsDirectoryWhenRootContainsCsvFile() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomerName()).thenReturn("csvCustomer");
		String previousBackupDirectory = UtilImpl.BACKUP_DIRECTORY;
		File root = Files.createTempDirectory("skyve-backup-util-").toFile();
		String extractName = "extract";
		File backupDirectory = new File(new File(root, "backup_csvCustomer"), extractName);
		assertTrue(backupDirectory.mkdirs());
		File csv = new File(backupDirectory, "Document.CSV");
		assertTrue(csv.createNewFile());
		UtilImpl.BACKUP_DIRECTORY = root.getAbsolutePath() + File.separator;

		try {
			withThreadLocalPersistence(persistence, () -> {
				File result = BackupUtil.validateSkyveBackup(extractName);

				assertThat(result, is(backupDirectory));
			});
		}
		finally {
			UtilImpl.BACKUP_DIRECTORY = previousBackupDirectory;
			Files.delete(csv.toPath());
			Files.delete(backupDirectory.toPath());
			Files.delete(backupDirectory.getParentFile().toPath());
			Files.delete(root.toPath());
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void privateRedactStringReturnsNullForNullInput() throws Exception {
		assertThat(invokePrivate("redactString", new Class<?>[] { String.class, Integer.class }, null, Integer.valueOf(10)), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void privateRedactNumericReturnsNullForNullInputAndUnsupportedNumber() throws Exception {
		assertThat(invokePrivate("redactNumeric", new Class<?>[] { Number.class }, new Object[] { null }), is(nullValue()));
		assertThat(invokePrivate("redactNumeric", new Class<?>[] { Number.class }, Short.valueOf((short) 7)), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void privateRedactGeometryReturnsOriginalForUnsupportedGeometryType() throws Exception {
		Geometry geometry = new GeometryFactory().createMultiPointFromCoords(new Coordinate[] { new Coordinate(1.2, 3.4), new Coordinate(5.6, 7.8) });

		Object result = invokePrivate("redactGeometry", new Class<?>[] { Geometry.class }, geometry);

		assertThat(result, is(geometry));
	}

	@Test
	@SuppressWarnings("static-method")
	public void executeScriptThrowsWhenPersistenceIsNotHibernate() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		SQL sql = mock(SQL.class);
		when(persistence.newSQL("select 1")).thenReturn(sql);
		when(sql.noTimeout()).thenReturn(sql);

		withThreadLocalPersistence(persistence, () -> {
			List<String> script = Collections.singletonList("select 1");
			try {
				BackupUtil.executeScript(script);
			}
			catch (ClassCastException e) {
				assertThat(e.getMessage(), containsString("AbstractHibernatePersistence"));
				return;
			}
			org.junit.Assert.fail("Expected ClassCastException");
		});
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	public void getTablesAddsJoinTableForPolymorphicReference() throws Exception {
		ProvidedRepository previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);

		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		CustomerImpl customer = mock(CustomerImpl.class);
		Module module = mock(Module.class);
		DocumentRef documentRef = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Persistent persistent = mock(Persistent.class);

		Document referencedDocument = mock(Document.class);
		org.skyve.metadata.model.document.Collection collection = mock(org.skyve.metadata.model.document.Collection.class);
		Persistent ownerPersistent = mock(Persistent.class);

		Document derivedDocument = mock(Document.class);
		Persistent derivedPersistent = mock(Persistent.class);

		ExportedReference reference = new ExportedReference();
		reference.setModuleName("mod");
		reference.setDocumentName("RefDoc");
		reference.setReferenceFieldName("items");
		reference.setType(CollectionType.composition);
		reference.setPersistent(ownerPersistent);

		Map<String, DocumentRef> refs = new HashMap<>();
		refs.put("MyDoc", documentRef);

		org.skyve.metadata.model.Attribute.Sensitivity sensitivity = org.skyve.metadata.model.Attribute.Sensitivity.none;

		try {
			when(persistence.getUser()).thenReturn(user);
			when(user.getCustomer()).thenReturn(customer);
			when(customer.getModules()).thenReturn(Collections.singletonList(module));
			when(customer.getModule("mod")).thenReturn(module);
			when(customer.getExportedReferences(document)).thenReturn(Collections.singletonList(reference));
			when(customer.getDerivedDocuments(referencedDocument)).thenReturn(Collections.singletonList("mod.DerivedDoc"));
			when(module.getName()).thenReturn("mod");
			when(module.getDocumentRefs()).thenReturn(refs);
			when(module.getDocument(customer, "MyDoc")).thenReturn(document);
			when(module.getDocument(customer, "RefDoc")).thenReturn(referencedDocument);
			when(module.getDocument(customer, "DerivedDoc")).thenReturn(derivedDocument);
			when(documentRef.getOwningModuleName()).thenReturn("mod");
			when(document.isDynamic()).thenReturn(false);
			when(document.isPersistable()).thenReturn(true);
			when(document.getPersistent()).thenReturn(persistent);
			when(document.getOwningModuleName()).thenReturn("mod");
			when(document.getAllAttributes(customer)).thenReturn(Collections.emptyList());
			when(document.getParentDocumentName()).thenReturn(null);
			when(document.isOrdered()).thenReturn(false);
			when(document.getBizKeySensitity()).thenReturn(sensitivity);
			when(document.getExtends()).thenReturn(null);
			when(persistent.getAgnosticIdentifier()).thenReturn("T_DOC");
			when(persistent.getPersistentIdentifier()).thenReturn("T_DOC");
			when(repository.findNearestPersistentSingleOrJoinedSuperDocument(customer, module, document)).thenReturn(document);

			when(referencedDocument.isDynamic()).thenReturn(false);
			when(referencedDocument.getReferenceByName("items")).thenReturn(collection);
			when(collection.isPersistent()).thenReturn(true);
			when(collection.getOrdered()).thenReturn(Boolean.TRUE);

			when(ownerPersistent.getAgnosticIdentifier()).thenReturn("T_OWNER");
			when(ownerPersistent.getPersistentIdentifier()).thenReturn("T_OWNER");
			when(ownerPersistent.getStrategy()).thenReturn(null);
			when(ownerPersistent.isPolymorphicallyMapped()).thenReturn(true);

			when(derivedDocument.isPersistable()).thenReturn(true);
			when(derivedDocument.getPersistent()).thenReturn(derivedPersistent);
			when(derivedPersistent.getAgnosticIdentifier()).thenReturn("T_DERIVED");
			when(derivedPersistent.getPersistentIdentifier()).thenReturn("T_DERIVED");

			withThreadLocalPersistence(persistence, () -> {
				Collection<Table> tables = BackupUtil.getTables();
				assertEquals(2, tables.size());
			});
		}
		finally {
			ProvidedRepositoryFactory.set(previousRepository);
		}
	}

	private static void withThreadLocalPersistence(AbstractPersistence persistence, ThrowingRunnable runnable) throws Exception {
		ThreadLocal<AbstractPersistence> threadLocal = getThreadLocalPersistence();
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			runnable.run();
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}

	private static ThreadLocal<AbstractPersistence> getThreadLocalPersistence() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		return threadLocal;
	}

	private static Object invokePrivate(String name, Class<?>[] parameterTypes, Object... args) throws Exception {
		Method method = BackupUtil.class.getDeclaredMethod(name, parameterTypes);
		method.setAccessible(true);
		return method.invoke(null, args);
	}

	@FunctionalInterface
	private interface ThrowingRunnable {
		void run() throws Exception;
	}
}
