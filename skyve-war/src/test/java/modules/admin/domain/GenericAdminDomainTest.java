package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

/**
 * Tests for the {@link Generic} admin domain bean (transient child).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class GenericAdminDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesGenericBean() {
		Generic bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Generic.MODULE_NAME, Generic.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		Generic bean = new Generic();
		assertEquals(Generic.MODULE_NAME, bean.getBizModule());
		assertEquals(Generic.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void idPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setId1("id1"); bean.setId2("id2"); bean.setId3("id3"); bean.setId4("id4"); bean.setId5("id5");
		bean.setId6("id6"); bean.setId7("id7"); bean.setId8("id8"); bean.setId9("id9"); bean.setId10("id10");
		assertNotNull(bean.getId1()); assertNotNull(bean.getId2()); assertNotNull(bean.getId3());
		assertNotNull(bean.getId4()); assertNotNull(bean.getId5()); assertNotNull(bean.getId6());
		assertNotNull(bean.getId7()); assertNotNull(bean.getId8()); assertNotNull(bean.getId9());
		assertNotNull(bean.getId10());
	}

	@Test
	void memoPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setMemo1("memo1"); bean.setMemo2("memo2"); bean.setMemo3("memo3"); bean.setMemo4("memo4");
		bean.setMemo5("memo5"); bean.setMemo6("memo6"); bean.setMemo7("memo7"); bean.setMemo8("memo8");
		bean.setMemo9("memo9"); bean.setMemo10("memo10");
		assertNotNull(bean.getMemo1()); assertNotNull(bean.getMemo2()); assertNotNull(bean.getMemo3());
		assertNotNull(bean.getMemo4()); assertNotNull(bean.getMemo5()); assertNotNull(bean.getMemo6());
		assertNotNull(bean.getMemo7()); assertNotNull(bean.getMemo8()); assertNotNull(bean.getMemo9());
		assertNotNull(bean.getMemo10());
	}

	@Test
	void booleanPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setBoolean1(Boolean.TRUE); bean.setBoolean2(Boolean.FALSE); bean.setBoolean3(Boolean.TRUE);
		bean.setBoolean4(Boolean.FALSE); bean.setBoolean5(Boolean.TRUE); bean.setBoolean6(Boolean.FALSE);
		bean.setBoolean7(Boolean.TRUE); bean.setBoolean8(Boolean.FALSE); bean.setBoolean9(Boolean.TRUE);
		bean.setBoolean10(Boolean.FALSE);
		assertNotNull(bean.getBoolean1()); assertNotNull(bean.getBoolean2()); assertNotNull(bean.getBoolean3());
		assertNotNull(bean.getBoolean4()); assertNotNull(bean.getBoolean5()); assertNotNull(bean.getBoolean6());
		assertNotNull(bean.getBoolean7()); assertNotNull(bean.getBoolean8()); assertNotNull(bean.getBoolean9());
		assertNotNull(bean.getBoolean10());
	}

	@Test
	void datePropertiesSetAndGet() {
		Generic bean = new Generic();
		DateOnly d = new DateOnly();
		bean.setDate1(d); bean.setDate2(d); bean.setDate3(d); bean.setDate4(d); bean.setDate5(d);
		bean.setDate6(d); bean.setDate7(d); bean.setDate8(d); bean.setDate9(d); bean.setDate10(d);
		assertNotNull(bean.getDate1()); assertNotNull(bean.getDate2()); assertNotNull(bean.getDate3());
		assertNotNull(bean.getDate4()); assertNotNull(bean.getDate5()); assertNotNull(bean.getDate6());
		assertNotNull(bean.getDate7()); assertNotNull(bean.getDate8()); assertNotNull(bean.getDate9());
		assertNotNull(bean.getDate10());
	}

	@Test
	void dateTimePropertiesSetAndGet() {
		Generic bean = new Generic();
		DateTime dt = new DateTime();
		bean.setDateTime1(dt); bean.setDateTime2(dt); bean.setDateTime3(dt); bean.setDateTime4(dt);
		bean.setDateTime5(dt); bean.setDateTime6(dt); bean.setDateTime7(dt); bean.setDateTime8(dt);
		bean.setDateTime9(dt); bean.setDateTime10(dt);
		assertNotNull(bean.getDateTime1()); assertNotNull(bean.getDateTime2()); assertNotNull(bean.getDateTime3());
		assertNotNull(bean.getDateTime4()); assertNotNull(bean.getDateTime5()); assertNotNull(bean.getDateTime6());
		assertNotNull(bean.getDateTime7()); assertNotNull(bean.getDateTime8()); assertNotNull(bean.getDateTime9());
		assertNotNull(bean.getDateTime10());
	}

	@Test
	void timestampPropertiesSetAndGet() {
		Generic bean = new Generic();
		Timestamp ts = new Timestamp();
		bean.setTimestamp1(ts); bean.setTimestamp2(ts); bean.setTimestamp3(ts); bean.setTimestamp4(ts);
		bean.setTimestamp5(ts); bean.setTimestamp6(ts); bean.setTimestamp7(ts); bean.setTimestamp8(ts);
		bean.setTimestamp9(ts); bean.setTimestamp10(ts);
		assertNotNull(bean.getTimestamp1()); assertNotNull(bean.getTimestamp2()); assertNotNull(bean.getTimestamp3());
		assertNotNull(bean.getTimestamp4()); assertNotNull(bean.getTimestamp5()); assertNotNull(bean.getTimestamp6());
		assertNotNull(bean.getTimestamp7()); assertNotNull(bean.getTimestamp8()); assertNotNull(bean.getTimestamp9());
		assertNotNull(bean.getTimestamp10());
	}

	@Test
	void timePropertiesSetAndGet() {
		Generic bean = new Generic();
		TimeOnly t = new TimeOnly();
		bean.setTime1(t); bean.setTime2(t); bean.setTime3(t); bean.setTime4(t); bean.setTime5(t);
		bean.setTime6(t); bean.setTime7(t); bean.setTime8(t); bean.setTime9(t); bean.setTime10(t);
		assertNotNull(bean.getTime1()); assertNotNull(bean.getTime2()); assertNotNull(bean.getTime3());
		assertNotNull(bean.getTime4()); assertNotNull(bean.getTime5()); assertNotNull(bean.getTime6());
		assertNotNull(bean.getTime7()); assertNotNull(bean.getTime8()); assertNotNull(bean.getTime9());
		assertNotNull(bean.getTime10());
	}

	@Test
	void decimal2PropertiesSetAndGet() {
		Generic bean = new Generic();
		Decimal2 v = new Decimal2("1.23");
		bean.setDecimal21(v); bean.setDecimal22(v); bean.setDecimal23(v); bean.setDecimal24(v);
		bean.setDecimal25(v); bean.setDecimal26(v); bean.setDecimal27(v); bean.setDecimal28(v);
		bean.setDecimal29(v); bean.setDecimal210(v);
		assertNotNull(bean.getDecimal21()); assertNotNull(bean.getDecimal22()); assertNotNull(bean.getDecimal23());
		assertNotNull(bean.getDecimal24()); assertNotNull(bean.getDecimal25()); assertNotNull(bean.getDecimal26());
		assertNotNull(bean.getDecimal27()); assertNotNull(bean.getDecimal28()); assertNotNull(bean.getDecimal29());
		assertNotNull(bean.getDecimal210());
	}

	@Test
	void decimal5PropertiesSetAndGet() {
		Generic bean = new Generic();
		Decimal5 v = new Decimal5("1.23456");
		bean.setDecimal51(v); bean.setDecimal52(v); bean.setDecimal53(v); bean.setDecimal54(v);
		bean.setDecimal55(v); bean.setDecimal56(v); bean.setDecimal57(v); bean.setDecimal58(v);
		bean.setDecimal59(v); bean.setDecimal510(v);
		assertNotNull(bean.getDecimal51()); assertNotNull(bean.getDecimal52()); assertNotNull(bean.getDecimal53());
		assertNotNull(bean.getDecimal54()); assertNotNull(bean.getDecimal55()); assertNotNull(bean.getDecimal56());
		assertNotNull(bean.getDecimal57()); assertNotNull(bean.getDecimal58()); assertNotNull(bean.getDecimal59());
		assertNotNull(bean.getDecimal510());
	}

	@Test
	void decimal10PropertiesSetAndGet() {
		Generic bean = new Generic();
		Decimal10 v = new Decimal10("1.1234567890");
		bean.setDecimal101(v); bean.setDecimal102(v); bean.setDecimal103(v); bean.setDecimal104(v);
		bean.setDecimal105(v); bean.setDecimal106(v); bean.setDecimal107(v); bean.setDecimal108(v);
		bean.setDecimal109(v); bean.setDecimal1010(v);
		assertNotNull(bean.getDecimal101()); assertNotNull(bean.getDecimal102()); assertNotNull(bean.getDecimal103());
		assertNotNull(bean.getDecimal104()); assertNotNull(bean.getDecimal105()); assertNotNull(bean.getDecimal106());
		assertNotNull(bean.getDecimal107()); assertNotNull(bean.getDecimal108()); assertNotNull(bean.getDecimal109());
		assertNotNull(bean.getDecimal1010());
	}

	@Test
	void integerPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setInteger1(Integer.valueOf(1)); bean.setInteger2(Integer.valueOf(2)); bean.setInteger3(Integer.valueOf(3));
		bean.setInteger4(Integer.valueOf(4)); bean.setInteger5(Integer.valueOf(5)); bean.setInteger6(Integer.valueOf(6));
		bean.setInteger7(Integer.valueOf(7)); bean.setInteger8(Integer.valueOf(8)); bean.setInteger9(Integer.valueOf(9));
		bean.setInteger10(Integer.valueOf(10));
		assertNotNull(bean.getInteger1()); assertNotNull(bean.getInteger2()); assertNotNull(bean.getInteger3());
		assertNotNull(bean.getInteger4()); assertNotNull(bean.getInteger5()); assertNotNull(bean.getInteger6());
		assertNotNull(bean.getInteger7()); assertNotNull(bean.getInteger8()); assertNotNull(bean.getInteger9());
		assertNotNull(bean.getInteger10());
	}

	@Test
	void longIntegerPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setLongInteger1(Long.valueOf(1L)); bean.setLongInteger2(Long.valueOf(2L)); bean.setLongInteger3(Long.valueOf(3L));
		bean.setLongInteger4(Long.valueOf(4L)); bean.setLongInteger5(Long.valueOf(5L)); bean.setLongInteger6(Long.valueOf(6L));
		bean.setLongInteger7(Long.valueOf(7L)); bean.setLongInteger8(Long.valueOf(8L)); bean.setLongInteger9(Long.valueOf(9L));
		bean.setLongInteger10(Long.valueOf(10L));
		assertNotNull(bean.getLongInteger1()); assertNotNull(bean.getLongInteger2()); assertNotNull(bean.getLongInteger3());
		assertNotNull(bean.getLongInteger4()); assertNotNull(bean.getLongInteger5()); assertNotNull(bean.getLongInteger6());
		assertNotNull(bean.getLongInteger7()); assertNotNull(bean.getLongInteger8()); assertNotNull(bean.getLongInteger9());
		assertNotNull(bean.getLongInteger10());
	}

	@Test
	void markupPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setMarkup1("m1"); bean.setMarkup2("m2"); bean.setMarkup3("m3"); bean.setMarkup4("m4"); bean.setMarkup5("m5");
		bean.setMarkup6("m6"); bean.setMarkup7("m7"); bean.setMarkup8("m8"); bean.setMarkup9("m9"); bean.setMarkup10("m10");
		assertNotNull(bean.getMarkup1()); assertNotNull(bean.getMarkup2()); assertNotNull(bean.getMarkup3());
		assertNotNull(bean.getMarkup4()); assertNotNull(bean.getMarkup5()); assertNotNull(bean.getMarkup6());
		assertNotNull(bean.getMarkup7()); assertNotNull(bean.getMarkup8()); assertNotNull(bean.getMarkup9());
		assertNotNull(bean.getMarkup10());
	}

	@Test
	void text500PropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setText5001("a"); bean.setText5002("b"); bean.setText5003("c"); bean.setText5004("d"); bean.setText5005("e");
		bean.setText5006("f"); bean.setText5007("g"); bean.setText5008("h"); bean.setText5009("i"); bean.setText50010("j");
		assertNotNull(bean.getText5001()); assertNotNull(bean.getText5002()); assertNotNull(bean.getText5003());
		assertNotNull(bean.getText5004()); assertNotNull(bean.getText5005()); assertNotNull(bean.getText5006());
		assertNotNull(bean.getText5007()); assertNotNull(bean.getText5008()); assertNotNull(bean.getText5009());
		assertNotNull(bean.getText50010());
	}

	@Test
	void hierarchicalPropertiesSetAndGet() {
		Generic bean = new Generic();
		bean.setBizParentId("parent-1");
		assertNotNull(bean.getBizParentId());
	}
}
