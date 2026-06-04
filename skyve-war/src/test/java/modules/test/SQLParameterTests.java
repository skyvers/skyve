package modules.test;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesPersistent.Enum3;

@SuppressWarnings("java:S8692") // system clock OK
class SQLParameterTests extends AbstractSkyveTest {

	private static BigDecimal largeFloater = new BigDecimal(0.123456789123456789);
	
	private String persistentIdentifier;
	
	@Override
	@BeforeEach
	void before() {
		super.before();
		persistentIdentifier = aapd.getPersistent().getPersistentIdentifier();
	}
	
	@Test
	void testBeanParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where bizId = :param", persistentIdentifier));
		sql.putParameter("param", aap);
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectBeanAssociationParam() throws Exception {
		testBeanObject(AttributeType.association);
	}

	@Test
	void testObjectBeanIdParam() throws Exception {
		testBeanObject(AttributeType.id);
	}

	private void testBeanObject(AttributeType type) throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		// test scalar
		SQL sql = p.newSQL(aapd, String.format("select * from %s where bizId = :param", persistentIdentifier));
		sql.putParameter("param", aap, type);
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());

		// test null
		sql = p.newSQL(aapd, String.format("select * from %s where bizid = :param", persistentIdentifier));
		sql.putParameter("param", null, type);
		results = sql.beanResults();
		Assert.assertEquals(0, results.size());

		// test array
		sql = p.newSQL(aapd, String.format("select * from %s where bizid in (:param)", persistentIdentifier));
		sql.putParameter("param", new Object[] { aap }, type);
		results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());

		// test collection
		sql = p.newSQL(aapd, String.format("select * from %s where bizId in (:param)", persistentIdentifier));
		sql.putParameter("param", Collections.singletonList(aap), type);
		results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testBooleanParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where booleanFlag = :param", persistentIdentifier));
		sql.putParameter("param", aap.getBooleanFlag());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectBooleanParam() throws Exception {
		testObject(AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE, AttributeType.bool);
	}

	@Test
	void testDateOnlyParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where date = :param", persistentIdentifier));
		sql.putParameter("param", aap.getDate());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectDateOnlyParam() throws Exception {
		testObject(AllAttributesPersistent.datePropertyName, new DateOnly(), AttributeType.date);
	}

	@Test
	void testDateOnlyParamCoersion() {
		testParamCoercion(AllAttributesPersistent.datePropertyName, new DateTime(), AttributeType.date);
		testParamCoercion(AllAttributesPersistent.datePropertyName, new TimeOnly(), AttributeType.date);
		testParamCoercion(AllAttributesPersistent.datePropertyName, new Timestamp(), AttributeType.date);
		testParamCoercion(AllAttributesPersistent.datePropertyName, new Date(), AttributeType.date);
		testParamCoercion(AllAttributesPersistent.datePropertyName, new java.sql.Date(System.currentTimeMillis()), AttributeType.date);
	}
	
	@Test
	void testDateTimeParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where dateTime = :param", persistentIdentifier));
		sql.putParameter("param", aap.getDateTime());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectDateTimeParam() throws Exception {
		testObject(AllAttributesPersistent.dateTimePropertyName, new DateTime(), AttributeType.dateTime);
	}

	@Test
	void testDateTimeParamCoersion() {
		testParamCoercion(AllAttributesPersistent.dateTimePropertyName, new DateOnly(), AttributeType.dateTime);
		testParamCoercion(AllAttributesPersistent.dateTimePropertyName, new TimeOnly(), AttributeType.dateTime);
		testParamCoercion(AllAttributesPersistent.dateTimePropertyName, new Timestamp(), AttributeType.dateTime);
		testParamCoercion(AllAttributesPersistent.dateTimePropertyName, new Date(), AttributeType.dateTime);
		testParamCoercion(AllAttributesPersistent.dateTimePropertyName, new java.sql.Date(System.currentTimeMillis()), AttributeType.dateTime);
	}

	@Test
	void testDecimal2Param() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where decimal2 = :param", persistentIdentifier));
		sql.putParameter("param", aap.getDecimal2());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectDecimal2Param() throws Exception {
		testObject(AllAttributesPersistent.decimal2PropertyName, new Decimal2(0), AttributeType.decimal2);
	}

	@Test
	void testDecimal2ParamCoersion() {
		testParamCoercion(AllAttributesPersistent.decimal2PropertyName, new Decimal5(largeFloater), AttributeType.decimal2);
		testParamCoercion(AllAttributesPersistent.decimal2PropertyName, new Decimal10(largeFloater), AttributeType.decimal2);
		testParamCoercion(AllAttributesPersistent.decimal2PropertyName, largeFloater, AttributeType.decimal2);
	}
	
	@Test
	void testDecimal5Param() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where decimal5 = :param", persistentIdentifier));
		sql.putParameter("param", aap.getDecimal5());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectDecimal5Param() throws Exception {
		testObject(AllAttributesPersistent.decimal5PropertyName, new Decimal5(0), AttributeType.decimal5);
	}

	@Test
	void testDecimal5ParamCoersion() {
		testParamCoercion(AllAttributesPersistent.decimal5PropertyName, new Decimal2(largeFloater), AttributeType.decimal5);
		testParamCoercion(AllAttributesPersistent.decimal5PropertyName, new Decimal10(largeFloater), AttributeType.decimal5);
		testParamCoercion(AllAttributesPersistent.decimal5PropertyName, largeFloater, AttributeType.decimal5);
	}

	@Test
	void testDecimal10Param() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where decimal10 = :param", persistentIdentifier));
		sql.putParameter("param", aap.getDecimal10());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectDecimal10Param() throws Exception {
		testObject(AllAttributesPersistent.decimal10PropertyName, new Decimal10(0), AttributeType.decimal10);
	}

	@Test
	void testDecimal10ParamCoersion() {
		testParamCoercion(AllAttributesPersistent.decimal10PropertyName, new Decimal2(largeFloater), AttributeType.decimal10);
		testParamCoercion(AllAttributesPersistent.decimal10PropertyName, new Decimal5(largeFloater), AttributeType.decimal10);
		testParamCoercion(AllAttributesPersistent.decimal10PropertyName, largeFloater, AttributeType.decimal10);
	}

	@Test
	void testEnumParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap.setEnum3(Enum3.one);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where enum3 = :param", persistentIdentifier));
		sql.putParameter("param", aap.getEnum3());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectEnumParam() throws Exception {
		testObject(AllAttributesPersistent.enum3PropertyName, Enum3.one, AttributeType.enumeration);
	}

	@Test
	void testEnumParamCoersion() {
		testParamCoercion(AllAttributesPersistent.enum3PropertyName, "one", AttributeType.enumeration);
	}

	@Test
	void testGeometryParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where geometry = :param", persistentIdentifier));
		sql.putParameter("param", aap.getGeometry());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectGeometryParam() throws Exception {
		testObject(AllAttributesPersistent.geometryPropertyName, new GeometryFactory().createPoint(new Coordinate(0, 0)),
				AttributeType.geometry);
	}

	@Test
	void testIntegerParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier, AllAttributesPersistent.normalIntegerPropertyName));
		sql.putParameter("param", aap.getNormalInteger());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectIntegerParam() throws Exception {
		testObject(AllAttributesPersistent.normalIntegerPropertyName, Integer.valueOf(0), AttributeType.integer);
	}

	@Test
	void testLongParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where longInteger = :param", persistentIdentifier));
		sql.putParameter("param", aap.getLongInteger());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectLongParam() throws Exception {
		testObject(AllAttributesPersistent.longIntegerPropertyName, Long.valueOf(0), AttributeType.longInteger);
	}

	@Test
	void testTimeOnlyParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where time = :param", persistentIdentifier));
		sql.putParameter("param", aap.getTime());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectTimeOnlyParam() throws Exception {
		testObject(AllAttributesPersistent.timePropertyName, new TimeOnly(), AttributeType.time);
	}

	@Test
	void testTimeOnlyParamCoersion() {
		testParamCoercion(AllAttributesPersistent.timePropertyName, new DateTime(), AttributeType.time);
		testParamCoercion(AllAttributesPersistent.timePropertyName, new DateOnly(), AttributeType.time);
		testParamCoercion(AllAttributesPersistent.timePropertyName, new Timestamp(), AttributeType.time);
		testParamCoercion(AllAttributesPersistent.timePropertyName, new Date(), AttributeType.time);
		testParamCoercion(AllAttributesPersistent.timePropertyName, new java.sql.Date(System.currentTimeMillis()), AttributeType.time);
	}

	@Test
	void testTimestampParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where timestamp = :param", persistentIdentifier));
		sql.putParameter("param", aap.getTimestamp());
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectTimestampParam() throws Exception {
		testObject(AllAttributesPersistent.timestampPropertyName, new Timestamp(), AttributeType.timestamp);
	}

	@Test
	void testTimestampParamCoersion() {
		testParamCoercion(AllAttributesPersistent.timestampPropertyName, new TimeOnly(), AttributeType.timestamp);
		testParamCoercion(AllAttributesPersistent.timestampPropertyName, new DateTime(), AttributeType.timestamp);
		testParamCoercion(AllAttributesPersistent.timestampPropertyName, new DateOnly(), AttributeType.timestamp);
		testParamCoercion(AllAttributesPersistent.timestampPropertyName, new Date(), AttributeType.timestamp);
		testParamCoercion(AllAttributesPersistent.timestampPropertyName, new java.sql.Date(System.currentTimeMillis()), AttributeType.timestamp);
	}

	@Test
	void testTextParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where text = :param", persistentIdentifier));
		sql.putParameter("param", aap.getText(), false);
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectTextParam() throws Exception {
		testObject(AllAttributesPersistent.textPropertyName, "TEST", AttributeType.text);
	}

	@Test
	void testMemoParam() throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		SQL sql = p.newSQL(aapd, String.format("select * from %s where memo = :param", persistentIdentifier));
		sql.putParameter("param", aap.getMemo(), true);
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}

	@Test
	void testObjectMemoParam() throws Exception {
		testObject(AllAttributesPersistent.memoPropertyName, "TEST", AttributeType.memo);
	}

	private void testParamCoercion(String binding, Object value, AttributeType type) {
		// test scalar
		SQL sql = p.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier, binding));
		sql.putParameter("param", value, type);
		sql.beanResults();
	}

	private void testObject(String binding, Object value, AttributeType type) throws Exception {
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		Binder.set(aap, binding, value);
		aap = p.save(aap);

		// test scalar
		SQL sql = p.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier, binding));
		sql.putParameter("param", value, type);
		List<AllAttributesPersistent> results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());

		// test null
		sql = p.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier, binding));
		sql.putParameter("param", null, type);
		results = sql.beanResults();
		Assert.assertEquals(0, results.size());

		// test array
		sql = p.newSQL(aapd, String.format("select * from %s where %s in (:param)", persistentIdentifier, binding));
		sql.putParameter("param", new Object[] { value }, type);
		results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());

		// test collection
		sql = p.newSQL(aapd, String.format("select * from %s where %s in (:param)", persistentIdentifier, binding));
		sql.putParameter("param", Collections.singletonList(value), type);
		results = sql.beanResults();
		Assert.assertEquals(1, results.size());
		Assert.assertEquals(aap.getBizId(), results.get(0).getBizId());
	}
}
