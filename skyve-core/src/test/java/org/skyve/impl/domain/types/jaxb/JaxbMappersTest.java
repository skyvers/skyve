package org.skyve.impl.domain.types.jaxb;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;

/** Unit tests for simple JAXB XmlAdapter mapper classes. */
@SuppressWarnings("static-method")
class JaxbMappersTest {

	// ---- DateOnlyMapper ----

	@Test
	void dateOnlyMapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new DateOnlyMapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void dateOnlyMapperUnmarshalDateReturnsDateOnly() throws Exception {
		DateOnly result = new DateOnlyMapper().unmarshal(new Date(0));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void dateOnlyMapperMarshalReturnsDateOnly() throws Exception {
		DateOnly value = new DateOnly(0);
		assertThat(new DateOnlyMapper().marshal(value), is(value));
	}

	// ---- DateTimeMapper ----

	@Test
	void dateTimeMapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new DateTimeMapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void dateTimeMapperUnmarshalDateReturnsDateTime() throws Exception {
		DateTime result = new DateTimeMapper().unmarshal(new Date(0));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void dateTimeMapperMarshalReturnsDateTime() throws Exception {
		DateTime value = new DateTime(0);
		assertThat(new DateTimeMapper().marshal(value), is(value));
	}

	// ---- TimeOnlyMapper ----

	@Test
	void timeOnlyMapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new TimeOnlyMapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void timeOnlyMapperUnmarshalDateReturnsTimeOnly() throws Exception {
		TimeOnly result = new TimeOnlyMapper().unmarshal(new Date(0));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void timeOnlyMapperMarshalReturnsTimeOnly() throws Exception {
		TimeOnly value = new TimeOnly(0);
		assertThat(new TimeOnlyMapper().marshal(value), is(value));
	}

	// ---- TimestampMapper ----

	@Test
	void timestampMapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new TimestampMapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void timestampMapperUnmarshalDateReturnsTimestamp() throws Exception {
		Timestamp result = new TimestampMapper().unmarshal(new Date(0));
		assertThat(result, is(notNullValue()));
	}

	@Test
	void timestampMapperMarshalReturnsTimestamp() throws Exception {
		Timestamp value = new Timestamp(0);
		assertThat(new TimestampMapper().marshal(value), is(value));
	}

	// ---- OptimisticLockMapper ----

	@Test
	void optimisticLockMapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new OptimisticLockMapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void optimisticLockMapperUnmarshalStringReturnsLock() throws Exception {
		OptimisticLock lock = new OptimisticLock("admin", new Date(0));
		OptimisticLock result = new OptimisticLockMapper().unmarshal(lock.toString());
		assertThat(result, is(notNullValue()));
	}

	@Test
	void optimisticLockMapperMarshalNullReturnsNull() throws Exception {
		assertThat(new OptimisticLockMapper().marshal(null), is(nullValue()));
	}

	@Test
	void optimisticLockMapperMarshalLockReturnsString() throws Exception {
		OptimisticLock lock = new OptimisticLock("admin", new Date(0));
		String result = new OptimisticLockMapper().marshal(lock);
		assertThat(result, is(notNullValue()));
	}

	// ---- Decimal2Mapper ----

	@Test
	void decimal2MapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal2Mapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void decimal2MapperUnmarshalValueReturnsDecimal2() throws Exception {
		Decimal2 result = new Decimal2Mapper().unmarshal(BigDecimal.TEN);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal2MapperMarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal2Mapper().marshal(null), is(nullValue()));
	}

	@Test
	void decimal2MapperMarshalValueReturnsBigDecimal() throws Exception {
		BigDecimal result = new Decimal2Mapper().marshal(new Decimal2("3.14"));
		assertThat(result, is(notNullValue()));
	}

	// ---- Decimal5Mapper ----

	@Test
	void decimal5MapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal5Mapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void decimal5MapperUnmarshalValueReturnsDecimal5() throws Exception {
		Decimal5 result = new Decimal5Mapper().unmarshal(BigDecimal.TEN);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal5MapperMarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal5Mapper().marshal(null), is(nullValue()));
	}

	@Test
	void decimal5MapperMarshalValueReturnsBigDecimal() throws Exception {
		BigDecimal result = new Decimal5Mapper().marshal(new Decimal5("1.23456"));
		assertThat(result, is(notNullValue()));
	}

	// ---- Decimal10Mapper ----

	@Test
	void decimal10MapperUnmarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal10Mapper().unmarshal(null), is(nullValue()));
	}

	@Test
	void decimal10MapperUnmarshalValueReturnsDecimal10() throws Exception {
		Decimal10 result = new Decimal10Mapper().unmarshal(BigDecimal.TEN);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void decimal10MapperMarshalNullReturnsNull() throws Exception {
		assertThat(new Decimal10Mapper().marshal(null), is(nullValue()));
	}

	@Test
	void decimal10MapperMarshalValueReturnsBigDecimal() throws Exception {
		BigDecimal result = new Decimal10Mapper().marshal(new Decimal10("9.9999999999"));
		assertThat(result, is(notNullValue()));
	}
}
