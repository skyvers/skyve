package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.math.BigDecimal;
import java.util.Date;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

@SuppressWarnings("static-method")
class BindUtilNullSafeConvertTest {

	// -----------------------------------------------------------------------
	// convert() - null guard
	// -----------------------------------------------------------------------

	@Test
	void convertReturnsNullForNullValue() {
		assertNull(BindUtil.convert(Integer.class, null));
	}

	@Test
	void convertDelegatesToNullSafeConvert() {
		Object result = BindUtil.convert(Integer.class, Long.valueOf(42L));
		assertEquals(Integer.valueOf(42), result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Integer
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertIntegerFromLong() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Long.valueOf(7L));
		assertInstanceOf(Integer.class, result);
		assertEquals(Integer.valueOf(7), result);
	}

	@Test
	void nullSafeConvertIntegerFromDouble() {
		Object result = BindUtil.nullSafeConvert(Integer.class, Double.valueOf(3.9));
		assertInstanceOf(Integer.class, result);
		assertEquals(Integer.valueOf(3), result);
	}

	@Test
	void nullSafeConvertIntegerAlreadyInteger() {
		Integer original = Integer.valueOf(5);
		Object result = BindUtil.nullSafeConvert(Integer.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Long
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertLongFromInteger() {
		Object result = BindUtil.nullSafeConvert(Long.class, Integer.valueOf(99));
		assertInstanceOf(Long.class, result);
		assertEquals(Long.valueOf(99L), result);
	}

	@Test
	void nullSafeConvertLongAlreadyLong() {
		Long original = Long.valueOf(100L);
		Object result = BindUtil.nullSafeConvert(Long.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Short
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertShortFromInteger() {
		Object result = BindUtil.nullSafeConvert(Short.class, Integer.valueOf(10));
		assertInstanceOf(Short.class, result);
		assertEquals(Short.valueOf((short) 10), result);
	}

	@Test
	void nullSafeConvertShortAlreadyShort() {
		Short original = Short.valueOf((short) 5);
		Object result = BindUtil.nullSafeConvert(Short.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Float
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertFloatFromInteger() {
		Object result = BindUtil.nullSafeConvert(Float.class, Integer.valueOf(4));
		assertInstanceOf(Float.class, result);
		assertEquals(Float.valueOf(4.0f), result);
	}

	@Test
	void nullSafeConvertFloatAlreadyFloat() {
		Float original = Float.valueOf(1.5f);
		Object result = BindUtil.nullSafeConvert(Float.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Double
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDoubleFromLong() {
		Object result = BindUtil.nullSafeConvert(Double.class, Long.valueOf(8L));
		assertInstanceOf(Double.class, result);
		assertEquals(Double.valueOf(8.0), result);
	}

	@Test
	void nullSafeConvertDoubleAlreadyDouble() {
		Double original = Double.valueOf(2.5);
		Object result = BindUtil.nullSafeConvert(Double.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – BigDecimal
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertBigDecimalFromInteger() {
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, Integer.valueOf(123));
		assertInstanceOf(BigDecimal.class, result);
		assertEquals(new BigDecimal("123"), result);
	}

	@Test
	void nullSafeConvertBigDecimalFromString() {
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, "45.67");
		assertInstanceOf(BigDecimal.class, result);
		assertEquals(new BigDecimal("45.67"), result);
	}

	@Test
	void nullSafeConvertBigDecimalAlreadyBigDecimal() {
		BigDecimal original = new BigDecimal("10.00");
		Object result = BindUtil.nullSafeConvert(BigDecimal.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Decimal2
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDecimal2FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal2.class, "12.34");
		assertInstanceOf(Decimal2.class, result);
	}

	@Test
	void nullSafeConvertDecimal2FromInteger() {
		Object result = BindUtil.nullSafeConvert(Decimal2.class, Integer.valueOf(5));
		assertInstanceOf(Decimal2.class, result);
	}

	@Test
	void nullSafeConvertDecimal2AlreadyDecimal2() {
		Decimal2 original = new Decimal2(1.0);
		Object result = BindUtil.nullSafeConvert(Decimal2.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Decimal5
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDecimal5FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal5.class, "9.87654");
		assertInstanceOf(Decimal5.class, result);
	}

	@Test
	void nullSafeConvertDecimal5AlreadyDecimal5() {
		Decimal5 original = new Decimal5(2.0);
		Object result = BindUtil.nullSafeConvert(Decimal5.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Decimal10
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDecimal10FromString() {
		Object result = BindUtil.nullSafeConvert(Decimal10.class, "1.2345678901");
		assertInstanceOf(Decimal10.class, result);
	}

	@Test
	void nullSafeConvertDecimal10AlreadyDecimal10() {
		Decimal10 original = new Decimal10(3.0);
		Object result = BindUtil.nullSafeConvert(Decimal10.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – DateOnly
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDateOnlyFromJavaUtilDate() {
		Date plainDate = new Date(1_000_000L);
		Object result = BindUtil.nullSafeConvert(DateOnly.class, plainDate);
		assertInstanceOf(DateOnly.class, result);
	}

	@Test
	void nullSafeConvertDateOnlyAlreadyDateOnly() {
		DateOnly original = new DateOnly(1_000_000L);
		Object result = BindUtil.nullSafeConvert(DateOnly.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – TimeOnly
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertTimeOnlyFromJavaUtilDate() {
		Date plainDate = new Date(2_000_000L);
		Object result = BindUtil.nullSafeConvert(TimeOnly.class, plainDate);
		assertInstanceOf(TimeOnly.class, result);
	}

	@Test
	void nullSafeConvertTimeOnlyAlreadyTimeOnly() {
		TimeOnly original = new TimeOnly(2_000_000L);
		Object result = BindUtil.nullSafeConvert(TimeOnly.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – DateTime
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertDateTimeFromJavaUtilDate() {
		Date plainDate = new Date(3_000_000L);
		Object result = BindUtil.nullSafeConvert(DateTime.class, plainDate);
		assertInstanceOf(DateTime.class, result);
	}

	@Test
	void nullSafeConvertDateTimeAlreadyDateTime() {
		DateTime original = new DateTime(3_000_000L);
		Object result = BindUtil.nullSafeConvert(DateTime.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Timestamp
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertTimestampFromJavaUtilDate() {
		Date plainDate = new Date(4_000_000L);
		Object result = BindUtil.nullSafeConvert(Timestamp.class, plainDate);
		assertInstanceOf(Timestamp.class, result);
	}

	@Test
	void nullSafeConvertTimestampAlreadyTimestamp() {
		Timestamp original = new Timestamp(4_000_000L);
		Object result = BindUtil.nullSafeConvert(Timestamp.class, original);
		assertSame(original, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Geometry (WKT)
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertGeometryFromWktPoint() {
		Object result = BindUtil.nullSafeConvert(Geometry.class, "POINT (10 20)");
		assertInstanceOf(Geometry.class, result);
		assertNotNull(result);
	}

	@Test
	void nullSafeConvertGeometryFromWktPolygon() {
		Object result = BindUtil.nullSafeConvert(Geometry.class, "POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))");
		assertInstanceOf(Geometry.class, result);
	}

	@Test
	void nullSafeConvertGeometryInvalidWktThrowsDomainException() {
		assertThrows(DomainException.class,
				() -> BindUtil.nullSafeConvert(Geometry.class, "NOT VALID WKT!!"));
	}

	@Test
	void nullSafeConvertGeometryFromNonStringReturnsSameObject() {
		// When value is not a String, the geometry branch does nothing – returns as-is
		Geometry mockGeometry = org.mockito.Mockito.mock(Geometry.class);
		Object result = BindUtil.nullSafeConvert(Geometry.class, mockGeometry);
		assertSame(mockGeometry, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – plain Java enum (Enum.valueOf)
	// -----------------------------------------------------------------------

	private enum PlainTestEnum { ALPHA, BETA, GAMMA }

	@Test
	void nullSafeConvertPlainEnumFromString() {
		Object result = BindUtil.nullSafeConvert(PlainTestEnum.class, "BETA");
		assertEquals(PlainTestEnum.BETA, result);
	}

	@Test
	void nullSafeConvertPlainEnumAlreadySameEnum() {
		Object result = BindUtil.nullSafeConvert(PlainTestEnum.class, PlainTestEnum.ALPHA);
		assertEquals(PlainTestEnum.ALPHA, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – Skyve Enumeration (fromCode / fromLocalisedDescription)
	// -----------------------------------------------------------------------

	private enum SkyveTestEnum implements Enumeration {
		VALUE1("code1", "Desc One"),
		VALUE2("code2", "Desc Two");

		private final String code;
		private final String desc;

		SkyveTestEnum(String code, String desc) {
			this.code = code;
			this.desc = desc;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return desc;
		}

		@Override
		public DomainValue toDomainValue() {
			return new DomainValue(code, desc);
		}

		public static SkyveTestEnum fromCode(String c) {
			for (SkyveTestEnum e : values()) {
				if (e.code.equals(c)) {
					return e;
				}
			}
			return null;
		}

		public static SkyveTestEnum fromLocalisedDescription(String d) {
			for (SkyveTestEnum e : values()) {
				if (e.desc.equals(d)) {
					return e;
				}
			}
			return null;
		}

		public static DomainValue[] toDomainValues() {
			SkyveTestEnum[] vals = values();
			DomainValue[] result = new DomainValue[vals.length];
			for (int i = 0; i < vals.length; i++) {
				result[i] = vals[i].toDomainValue();
			}
			return result;
		}
	}

	@Test
	void nullSafeConvertSkyveEnumFromCode() {
		Object result = BindUtil.nullSafeConvert(SkyveTestEnum.class, "code1");
		assertEquals(SkyveTestEnum.VALUE1, result);
	}

	@Test
	void nullSafeConvertSkyveEnumFromLocalisedDescription() {
		Object result = BindUtil.nullSafeConvert(SkyveTestEnum.class, "Desc Two");
		assertEquals(SkyveTestEnum.VALUE2, result);
	}

	@Test
	void nullSafeConvertSkyveEnumFromEnumerationValue() {
		// value is already an Enumeration instance – should convert via toCode
		Object result = BindUtil.nullSafeConvert(SkyveTestEnum.class, SkyveTestEnum.VALUE1);
		assertEquals(SkyveTestEnum.VALUE1, result);
	}

	// -----------------------------------------------------------------------
	// nullSafeConvert – unmatched type pass-through
	// -----------------------------------------------------------------------

	@Test
	void nullSafeConvertUnknownTypeReturnsValueUnchanged() {
		// StringBuilder is not a known conversion type – returned as-is
		StringBuilder sb = new StringBuilder("hello");
		Object result = BindUtil.nullSafeConvert(StringBuilder.class, sb);
		assertSame(sb, result);
	}
}
