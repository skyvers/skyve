package modules.test;

import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.EXT;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesPersistent.Enum3;

public class SQLDataAccessTests extends AbstractSkyveTestDispose {

	private String persistentIdentifier;
	
	@BeforeEach
	@Override
	@SuppressWarnings("null")
	public void before() {
		super.before();
		persistentIdentifier = aapd.getPersistent().getPersistentIdentifier();
	}
	
	@Test
	public void testBeanParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));

			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);

			sql = sda.newSQL(aapd, String.format("select * from %s where bizId = :param", persistentIdentifier));
			sql.putParameter("param", aap);
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectBeanAssociationParam() throws Exception {
		testBeanObject(AttributeType.association);
	}

	@Test
	public void testObjectBeanIdParam() throws Exception {
		testBeanObject(AttributeType.id);
	}

	private void testBeanObject(AttributeType type) throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));

			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);

			// test scalar
			sql = sda.newSQL(aapd, String.format("select * from %s where bizId = :param", persistentIdentifier));
			sql.putParameter("param", aap, type);
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
	
			// test null
			sql = sda.newSQL(aapd, String.format("select * from %s where bizid = :param", persistentIdentifier));
			sql.putParameter("param", null, type);
			results = sql.beanResults();
			Assert.assertEquals(0, results.size());
		}
	}

	@Test
	public void testBooleanParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where booleanFlag = :param", persistentIdentifier));
			sql.putParameter("param", aap.getBooleanFlag());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectBooleanParam() throws Exception {
		testObject(AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE, AttributeType.bool);
	}

	@Test
	public void testDateOnlyParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where date = :param", persistentIdentifier));
			sql.putParameter("param", aap.getDate());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectDateOnlyParam() throws Exception {
		testObject(AllAttributesPersistent.datePropertyName, new DateOnly(), AttributeType.date);
	}

	@Test
	public void testDateTimeParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where dateTime = :param", persistentIdentifier));
			sql.putParameter("param", aap.getDateTime());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectDateTimeParam() throws Exception {
		testObject(AllAttributesPersistent.dateTimePropertyName, new DateTime(), AttributeType.dateTime);
	}

	@Test
	public void testDecimal2Param() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where decimal2 = :param", persistentIdentifier));
			sql.putParameter("param", aap.getDecimal2());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectDecimal2Param() throws Exception {
		testObject(AllAttributesPersistent.decimal2PropertyName, new Decimal2(0), AttributeType.decimal2);
	}

	@Test
	public void testDecimal5Param() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where decimal5 = :param", persistentIdentifier));
			sql.putParameter("param", aap.getDecimal5());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectDecimal5Param() throws Exception {
		testObject(AllAttributesPersistent.decimal5PropertyName, new Decimal5(0), AttributeType.decimal5);
	}

	@Test
	public void testDecimal10Param() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where decimal10 = :param", persistentIdentifier));
			sql.putParameter("param", aap.getDecimal10());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectDecimal10Param() throws Exception {
		testObject(AllAttributesPersistent.decimal10PropertyName, new Decimal10(0), AttributeType.decimal10);
	}

	@Test
	public void testEnumParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap.setEnum3(Enum3.one);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where enum3 = :param", persistentIdentifier));
			sql.putParameter("param", aap.getEnum3());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectEnumParam() throws Exception {
		testObject(AllAttributesPersistent.enum3PropertyName, Enum3.one, AttributeType.enumeration);
	}

	@Test
	public void testGeometryParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where geometry = :param", persistentIdentifier));
			sql.putParameter("param", aap.getGeometry());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectGeometryParam() throws Exception {
		testObject(AllAttributesPersistent.geometryPropertyName, 
					new GeometryFactory().createPoint(new Coordinate(0, 0)),
					AttributeType.geometry);
	}

	@Test
	public void testIntegerParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where %s = :param",
														persistentIdentifier,
														AllAttributesPersistent.normalIntegerPropertyName));
			sql.putParameter("param", aap.getNormalInteger());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectIntegerParam() throws Exception {
		testObject(AllAttributesPersistent.normalIntegerPropertyName, Integer.valueOf(0), AttributeType.integer);
	}

	@Test
	public void testLongParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where longInteger = :param", persistentIdentifier));
			sql.putParameter("param", aap.getLongInteger());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectLongParam() throws Exception {
		testObject(AllAttributesPersistent.longIntegerPropertyName, Long.valueOf(0), AttributeType.longInteger);
	}

	@Test
	public void testTimeOnlyParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where time = :param", persistentIdentifier));
			sql.putParameter("param", aap.getTime());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectTimeOnlyParam() throws Exception {
		testObject(AllAttributesPersistent.timePropertyName, new TimeOnly(), AttributeType.time);
	}

	@Test
	public void testTimestampParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where timestamp = :param", persistentIdentifier));
			sql.putParameter("param", aap.getTimestamp());
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectTimestampParam() throws Exception {
		testObject(AllAttributesPersistent.timestampPropertyName, new Timestamp(), AttributeType.timestamp);
	}

	@Test
	public void testTextParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			sql = sda.newSQL(aapd, String.format("select * from %s where text = :param", persistentIdentifier));
			sql.putParameter("param", aap.getText(), false);
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectTextParam() throws Exception {
		testObject(AllAttributesPersistent.textPropertyName, "TEST", AttributeType.text);
	}

	@Test
	public void testMemoParam() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);

			sql = sda.newSQL(aapd, String.format("select * from %s where memo = :param", persistentIdentifier));
			sql.putParameter("param", aap.getMemo(), true);
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
		}
	}

	@Test
	public void testObjectMemoParam() throws Exception {
		testObject(AllAttributesPersistent.memoPropertyName, "TEST", AttributeType.memo);
	}

	private void testObject(String binding, Object value, AttributeType type) throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			SQL sql = sda.newSQL(String.format("delete from %s", persistentIdentifier));
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			Binder.set(aap, binding, value);
			aap = p.save(aap);
			p.commit(false);

			// test scalar
			sql = sda.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier,
													binding));
			sql.putParameter("param", value, type);
			List<AllAttributesPersistent> results = sql.beanResults();
			Assert.assertEquals(1, results.size());
			Assert.assertNotEquals(aap.getBizId(), results.get(0).getBizId());
	
			// test null
			sql = sda.newSQL(aapd, String.format("select * from %s where %s = :param", persistentIdentifier, binding));
			sql.putParameter("param", null, type);
			results = sql.beanResults();
			Assert.assertEquals(0, results.size());
		}
	}

	@Test
	public void testSQLDyna() throws Exception {
		try (SQLDataAccess sda = EXT.newSQLDataAccess()) {
			sda.newSQL("delete from " + persistentIdentifier).execute();
	
			AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			aap = Util.constructRandomInstance(u, m, aapd, 1);
			aap = p.save(aap);
			p.commit(false);
	
			DynaBean bean = sda.newSQL("select bizId from " + persistentIdentifier).dynaResult();
			Assert.assertNotNull("bizId should not be null", Binder.get(bean, Bean.DOCUMENT_ID.toLowerCase()));

			List<DynaBean> beans = sda.newSQL("select bizId from " + persistentIdentifier).dynaResults();
			Assert.assertEquals("Requires some data", 3, beans.size());
			for (DynaBean thisBean : beans) {
				Assert.assertNotNull("bizId should not be null", Binder.get(thisBean, Bean.DOCUMENT_ID.toLowerCase()));
			}

			try (AutoClosingIterable<DynaBean> i = sda.newSQL("select bizId from " + persistentIdentifier).dynaIterable()) {
				boolean exists = false;
				for (DynaBean thisBean : i) {
					Assert.assertNotNull("bizId should not be null", Binder.get(thisBean, Bean.DOCUMENT_ID.toLowerCase()));
					exists = true;
				}
				Assert.assertTrue("Requires some data", exists);
			}

			beans = sda.newSQL("select bizId from " + persistentIdentifier + " where bizId = :bizId").putParameter(Bean.DOCUMENT_ID, "test", false).dynaResults();
			Assert.assertEquals("Should be no matches", 0, beans.size());
		}
	}
}
