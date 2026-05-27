package org.skyve.impl.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.domain.types.converters.time.HH_MI;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("boxing")
class ValidationUtilTest {

	@Mock
	private User user;

	@Mock
	private Bean bean;

	@Test
	void validateBeanPropertyInverseOneSkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.inverseOne);
		Mockito.when(attribute.getName()).thenReturn("someInverse");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanPropertyInverseManySkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.inverseMany);
		Mockito.when(attribute.getName()).thenReturn("someInverseMany");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanPropertyBizKeyBindingSkipsValidation() {
		Attribute attribute = Mockito.mock(Attribute.class);
		// Use a non-inverse type but the special bizKey binding name
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(attribute.getName()).thenReturn(Bean.BIZ_KEY);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanPropertyRequiredAttributeAddsMessageWhenValueNull() {
		// "bizId" is a valid Bean interface property — BindUtil.get(mockBean, "bizId") returns null
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(attribute.getName()).thenReturn("bizId");
		Mockito.when(attribute.isRequired()).thenReturn(Boolean.TRUE);
		Mockito.when(attribute.getLocalisedDisplayName()).thenReturn("ID");
		Mockito.when(attribute.getLocalisedRequiredMessage()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanPropertyRequiredAttributeWithCustomMessageAddsCustomMessage() {
		// Custom required message — exercises the non-null requiredMessage branch
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(attribute.getName()).thenReturn("bizId");
		Mockito.when(attribute.isRequired()).thenReturn(Boolean.TRUE);
		Mockito.when(attribute.getLocalisedDisplayName()).thenReturn("ID");
		Mockito.when(attribute.getLocalisedRequiredMessage()).thenReturn("Custom required message");
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertFalse(e.getMessages().isEmpty());
		assertTrue(e.getMessages().get(0).getText().contains("Custom required message"));
	}

	@Test
	void validateBeanPropertyNotRequiredNonNullValueAddsNoMessages() {
		// Not required, value present — no messages should be added
		Attribute attribute = Mockito.mock(Attribute.class);
		Mockito.when(attribute.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(attribute.getName()).thenReturn("bizId");
		Mockito.when(attribute.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(attribute.getLocalisedDisplayName()).thenReturn("ID");
		Mockito.when(bean.getBizId()).thenReturn("some-id");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, attribute, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateTextAttributeExceedingLengthAddsMessage() {
		// Text attribute with value exceeding max length
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Description");
		Mockito.when(text.getLength()).thenReturn(5);
		Mockito.when(text.getFormat()).thenReturn(null);
		Mockito.when(text.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn("toolongvalue");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateTextAttributeWithinLengthAddsNoMessage() {
		// Text attribute with value within max length
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Description");
		Mockito.when(text.getLength()).thenReturn(50);
		Mockito.when(text.getFormat()).thenReturn(null);
		Mockito.when(text.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn("short");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionMinCardinalityBelowMinAddsMessage() {
		// Collection with minCardinality=2 and empty collection → adds message
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("bizId");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(2);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionMinCardinalitySingularAddsMessage() {
		// Collection with minCardinality=1 → singular message branch
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("bizId");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(1);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionMaxCardinalityNullCollectionAddsNoMessage() {
		// maxCardinality=1 with null collection → no message (null check guards)
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("bizId");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(0);
		Mockito.when(collection.getMaxCardinality()).thenReturn(Integer.valueOf(1));
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionWithinCardinalityLimitsAddsNoMessage() {
		// Collection with minCardinality=0 and no max → no messages with null value
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("bizId");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(0);
		Mockito.when(collection.getMaxCardinality()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionExceedingPluralMaxCardinalityAddsMessage() {
		// 3 items exceeding maxCardinality=2 → plural message added
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("items");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(0);
		Mockito.when(collection.getMaxCardinality()).thenReturn(Integer.valueOf(2));

		List<Bean> items = new ArrayList<>();
		items.add(Mockito.mock(Bean.class));
		items.add(Mockito.mock(Bean.class));
		items.add(Mockito.mock(Bean.class));
		Mockito.when(bean.isDynamic("items")).thenReturn(Boolean.TRUE);
		Mockito.when(bean.getDynamic("items")).thenReturn(items);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateCollectionExceedingSingularMaxCardinalityAddsMessage() {
		// 2 items exceeding maxCardinality=1 → singular message added
		Collection collection = Mockito.mock(Collection.class);
		Mockito.when(collection.getAttributeType()).thenReturn(AttributeType.collection);
		Mockito.when(collection.getName()).thenReturn("items");
		Mockito.when(collection.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(collection.getLocalisedDisplayName()).thenReturn("Items");
		Mockito.when(collection.getMinCardinality()).thenReturn(0);
		Mockito.when(collection.getMaxCardinality()).thenReturn(Integer.valueOf(1));

		List<Bean> items = new ArrayList<>();
		items.add(Mockito.mock(Bean.class));
		items.add(Mockito.mock(Bean.class));
		Mockito.when(bean.isDynamic("items")).thenReturn(Boolean.TRUE);
		Mockito.when(bean.getDynamic("items")).thenReturn(items);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, collection, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateIntegerAttributeWithNullValidatorAddsNoMessage() {
		// Integer attribute with null validator — exercises the integer else-if branch
		org.skyve.impl.metadata.model.document.field.Integer intAttr =
			Mockito.mock(org.skyve.impl.metadata.model.document.field.Integer.class);
		Mockito.when(intAttr.getAttributeType()).thenReturn(AttributeType.integer);
		Mockito.when(intAttr.getName()).thenReturn("bizId");
		Mockito.when(intAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(intAttr.getLocalisedDisplayName()).thenReturn("Count");
		Mockito.when(intAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, intAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateLongIntegerAttributeWithNullValidatorAddsNoMessage() {
		// LongInteger attribute with null validator — exercises the longInteger else-if branch
		LongInteger longAttr = Mockito.mock(LongInteger.class);
		Mockito.when(longAttr.getAttributeType()).thenReturn(AttributeType.longInteger);
		Mockito.when(longAttr.getName()).thenReturn("bizId");
		Mockito.when(longAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(longAttr.getLocalisedDisplayName()).thenReturn("LongCount");
		Mockito.when(longAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, longAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDateAttributeWithNullValidatorAddsNoMessage() {
		// Date attribute with null validator — exercises the date else-if branch
		Date dateAttr = Mockito.mock(Date.class);
		Mockito.when(dateAttr.getAttributeType()).thenReturn(AttributeType.date);
		Mockito.when(dateAttr.getName()).thenReturn("bizId");
		Mockito.when(dateAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(dateAttr.getLocalisedDisplayName()).thenReturn("StartDate");
		Mockito.when(dateAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dateAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDateTimeAttributeWithNullValidatorAddsNoMessage() {
		// DateTime attribute with null validator — exercises the dateTime else-if branch
		DateTime dateTimeAttr = Mockito.mock(DateTime.class);
		Mockito.when(dateTimeAttr.getAttributeType()).thenReturn(AttributeType.dateTime);
		Mockito.when(dateTimeAttr.getName()).thenReturn("bizId");
		Mockito.when(dateTimeAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(dateTimeAttr.getLocalisedDisplayName()).thenReturn("CreatedAt");
		Mockito.when(dateTimeAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dateTimeAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateTimeAttributeWithNullValidatorAddsNoMessage() {
		// Time attribute with null validator — exercises the time else-if branch
		Time timeAttr = Mockito.mock(Time.class);
		Mockito.when(timeAttr.getAttributeType()).thenReturn(AttributeType.time);
		Mockito.when(timeAttr.getName()).thenReturn("bizId");
		Mockito.when(timeAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(timeAttr.getLocalisedDisplayName()).thenReturn("StartTime");
		Mockito.when(timeAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, timeAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateTimestampAttributeWithNullValidatorAddsNoMessage() {
		// Timestamp attribute with null validator — exercises the timestamp else-if branch
		Timestamp tsAttr = Mockito.mock(Timestamp.class);
		Mockito.when(tsAttr.getAttributeType()).thenReturn(AttributeType.timestamp);
		Mockito.when(tsAttr.getName()).thenReturn("bizId");
		Mockito.when(tsAttr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(tsAttr.getLocalisedDisplayName()).thenReturn("CreatedOn");
		Mockito.when(tsAttr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, tsAttr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal2AttributeWithNullValidatorAddsNoMessage() {
		// Decimal2 attribute with null validator — exercises the decimal2 else-if branch
		Decimal2 dec2Attr = Mockito.mock(Decimal2.class);
		Mockito.when(dec2Attr.getAttributeType()).thenReturn(AttributeType.decimal2);
		Mockito.when(dec2Attr.getName()).thenReturn("bizId");
		Mockito.when(dec2Attr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(dec2Attr.getLocalisedDisplayName()).thenReturn("Amount");
		Mockito.when(dec2Attr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec2Attr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal5AttributeWithNullValidatorAddsNoMessage() {
		// Decimal5 attribute with null validator — exercises the decimal5 else-if branch
		Decimal5 dec5Attr = Mockito.mock(Decimal5.class);
		Mockito.when(dec5Attr.getAttributeType()).thenReturn(AttributeType.decimal5);
		Mockito.when(dec5Attr.getName()).thenReturn("bizId");
		Mockito.when(dec5Attr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(dec5Attr.getLocalisedDisplayName()).thenReturn("Price");
		Mockito.when(dec5Attr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec5Attr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal10AttributeWithNullValidatorAddsNoMessage() {
		// Decimal10 attribute with null validator — exercises the decimal10 else-if branch
		Decimal10 dec10Attr = Mockito.mock(Decimal10.class);
		Mockito.when(dec10Attr.getAttributeType()).thenReturn(AttributeType.decimal10);
		Mockito.when(dec10Attr.getName()).thenReturn("bizId");
		Mockito.when(dec10Attr.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(dec10Attr.getLocalisedDisplayName()).thenReturn("Precision");
		Mockito.when(dec10Attr.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn(null);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec10Attr, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateIntegerAttributeWithMinViolationAddsMessage() {
		// Integer field with a real IntegerValidator (min=10) and value=5 → range violation
		org.skyve.impl.metadata.model.document.field.Integer intAttr =
				new org.skyve.impl.metadata.model.document.field.Integer();
		intAttr.setName("count");
		intAttr.setDisplayName("Count");

		IntegerValidator validator = new IntegerValidator();
		validator.setMin(java.lang.Integer.valueOf(10));
		intAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("count", java.lang.Integer.valueOf(5));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, intAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateIntegerAttributeWithinRangeAddsNoMessage() {
		// Integer field with a real IntegerValidator (max=100) and value=50 → no violation
		org.skyve.impl.metadata.model.document.field.Integer intAttr =
				new org.skyve.impl.metadata.model.document.field.Integer();
		intAttr.setName("count");
		intAttr.setDisplayName("Count");

		IntegerValidator validator = new IntegerValidator();
		validator.setMax(java.lang.Integer.valueOf(100));
		intAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("count", java.lang.Integer.valueOf(50));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, intAttr, dynBean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateLongIntegerAttributeWithMaxViolationAddsMessage() {
		// LongInteger field with a real LongValidator (max=100) and value=200 → range violation
		LongInteger longAttr = new LongInteger();
		longAttr.setName("amount");
		longAttr.setDisplayName("Amount");

		LongValidator validator = new LongValidator();
		validator.setMax(Long.valueOf(100L));
		longAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("amount", Long.valueOf(200L));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, longAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateLongIntegerAttributeWithinRangeAddsNoMessage() {
		// LongInteger field with a real LongValidator (min=0, max=1000) and value=500 → no violation
		LongInteger longAttr = new LongInteger();
		longAttr.setName("amount");
		longAttr.setDisplayName("Amount");

		LongValidator validator = new LongValidator();
		validator.setMin(Long.valueOf(0L));
		validator.setMax(Long.valueOf(1000L));
		longAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("amount", Long.valueOf(500L));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, longAttr, dynBean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateDateAttributeWithMaxViolationAddsMessage() {
		// Date field with a DateValidator (max=yesterday) and value=today → range violation
		Date dateAttr = new Date();
		dateAttr.setName("eventDate");
		dateAttr.setDisplayName("Event Date");

		dateAttr.setConverter(new DD_MMM_YYYY());
		DateValidator validator = new DateValidator();
		validator.setMax(new DateOnly(0L)); // epoch — any current date is after this
		dateAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("eventDate", new DateOnly()); // today
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dateAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateDateTimeAttributeWithMaxViolationAddsMessage() {
		DateTime dateTimeAttr = new DateTime();
		dateTimeAttr.setName("scheduledAt");
		dateTimeAttr.setDisplayName("Scheduled At");

		dateTimeAttr.setConverter(new DD_MMM_YYYY_HH_MI());
		DateValidator validator = new DateValidator();
		validator.setMax(new org.skyve.domain.types.DateTime(0L));
		dateTimeAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("scheduledAt", new org.skyve.domain.types.DateTime());
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dateTimeAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateTimeAttributeWithMaxViolationAddsMessage() {
		Time timeAttr = new Time();
		timeAttr.setName("startTime");
		timeAttr.setDisplayName("Start Time");

		timeAttr.setConverter(new HH_MI());
		DateValidator validator = new DateValidator();
		validator.setMax(new org.skyve.domain.types.TimeOnly(0, 0, 0)); // midnight local time
		timeAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("startTime", new org.skyve.domain.types.TimeOnly());
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, timeAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateTimestampAttributeWithMaxViolationAddsMessage() {
		Timestamp tsAttr = new Timestamp();
		tsAttr.setName("createdAt");
		tsAttr.setDisplayName("Created At");

		tsAttr.setConverter(new DD_MMM_YYYY_HH_MI_SS());
		DateValidator validator = new DateValidator();
		validator.setMax(new org.skyve.domain.types.Timestamp(0L));
		tsAttr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("createdAt", new org.skyve.domain.types.Timestamp());
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, tsAttr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal2AttributeWithMaxViolationAddsMessage() {
		Decimal2 dec2Attr = new Decimal2();
		dec2Attr.setName("price");
		dec2Attr.setDisplayName("Price");

		DecimalValidator validator = new DecimalValidator();
		validator.setMax(new org.skyve.domain.types.Decimal2(10.0));
		dec2Attr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("price", new org.skyve.domain.types.Decimal2(999.0));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec2Attr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal5AttributeWithMaxViolationAddsMessage() {
		Decimal5 dec5Attr = new Decimal5();
		dec5Attr.setName("ratio");
		dec5Attr.setDisplayName("Ratio");

		DecimalValidator validator = new DecimalValidator();
		validator.setMax(new org.skyve.domain.types.Decimal5(1.0));
		dec5Attr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("ratio", new org.skyve.domain.types.Decimal5(100.0));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec5Attr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateDecimal10AttributeWithMaxViolationAddsMessage() {
		Decimal10 dec10Attr = new Decimal10();
		dec10Attr.setName("balance");
		dec10Attr.setDisplayName("Balance");

		DecimalValidator validator = new DecimalValidator();
		validator.setMax(new org.skyve.domain.types.Decimal10(0.0));
		dec10Attr.setValidator(validator);

		java.util.Map<String, Object> props = new java.util.HashMap<>();
		props.put("balance", new org.skyve.domain.types.Decimal10(1000.0));
		DynamicBean dynBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, dec10Attr, dynBean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateTextAttributeWithFormatMaskViolationAddsMessage() {
		// Text attribute with TextFormat mask="###" and value "abc" → format violation in validateFormat
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Field");
		Mockito.when(text.getLength()).thenReturn(50);
		TextFormat textFormat = new TextFormat();
		textFormat.setMask("###");
		Mockito.when(text.getFormat()).thenReturn(textFormat);
		Mockito.when(text.getValidator()).thenReturn(null);
		Mockito.when(bean.getBizId()).thenReturn("abc");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateTextAttributeWithTextValidatorViolationAddsMessage() {
		// Text attribute with TextValidator (digits-only regex) and alphabetic value → adds message
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Field");
		Mockito.when(text.getLength()).thenReturn(100);
		Mockito.when(text.getFormat()).thenReturn(null);
		TextValidator tv = new TextValidator();
		tv.setRegularExpression("[0-9]+");
		Mockito.when(text.getValidator()).thenReturn(tv);
		Mockito.when(bean.getBizId()).thenReturn("notdigits");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, bean, e);

		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	@SuppressWarnings({"rawtypes", "unchecked"})
	void validateConverterWithNonNullValidatorCallsValidate() {
		// Converter with a non-null Validator → exercises line 132 (validator.validate call)
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Field");
		Mockito.when(text.getLength()).thenReturn(Integer.MAX_VALUE);
		Mockito.when(text.getFormat()).thenReturn(null);
		Mockito.when(text.getValidator()).thenReturn(null);

		org.skyve.domain.types.converters.Converter mockConverter = Mockito.mock(org.skyve.domain.types.converters.Converter.class);
		org.skyve.domain.types.converters.Validator mockValidator = Mockito.mock(org.skyve.domain.types.converters.Validator.class);
		Mockito.when(mockConverter.getValidator()).thenReturn(mockValidator);
		Mockito.when(mockConverter.getFormat()).thenReturn(null);
		org.skyve.metadata.customer.Customer mockCustomer = Mockito.mock(org.skyve.metadata.customer.Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);
		Mockito.when(text.getConverterForCustomer(mockCustomer)).thenReturn(mockConverter);
		Mockito.when(bean.getBizId()).thenReturn("someValue");

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, bean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	// ---- validateBeanAgainstDocument — covers L69-92 ----

	@Test
	void validateBeanAgainstDocumentWithNoAttributesDoesNotThrow() {
		// Covers L69 (CORE.getUser() path), L73-75 (empty loop), L79-88 (no inheritance), L90 (no failures)
		Customer mockCustomer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);

		Document document = Mockito.mock(Document.class);
		Mockito.when(document.getAttributes()).thenReturn(Collections.emptyList());
		Mockito.when(document.getExtends()).thenReturn(null);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			assertDoesNotThrow(() -> ValidationUtil.validateBeanAgainstDocument(document, bean));
			// Should complete without throwing
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	@Test
	void validateBeanAgainstDocumentThrowsWhenRequiredFieldMissing() {
		// Covers L91-92: LOGGER.warn + throw when validation fails
		Customer mockCustomer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);

		// A required Text attribute whose value is null → generates a validation message
		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("bizId");
		Mockito.when(text.isRequired()).thenReturn(Boolean.TRUE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("Name");
		Mockito.when(text.getLocalisedRequiredMessage()).thenReturn(null);
		Mockito.when(text.getConverterForCustomer(mockCustomer)).thenReturn(null);
		// bean.getBizId() returns null (default for Mockito mock)

		Document document = Mockito.mock(Document.class);
		List<Attribute> attrs = new ArrayList<>();
		attrs.add(text);
		Mockito.doReturn(attrs).when(document).getAttributes();
		Mockito.when(document.getExtends()).thenReturn(null);

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			org.junit.jupiter.api.Assertions.assertThrows(ValidationException.class,
					() -> ValidationUtil.validateBeanAgainstDocument(document, bean));
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	// ---- validateFormat — covers L276-303 ----

	@Test
	void validateBeanPropertyTextWithUpperCaseFormatReformatsValue() {
		// Covers validateFormat L276-283: success path where value is reformatted
		Customer mockCustomer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);

		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("myField");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("My Field");
		Mockito.when(text.getLength()).thenReturn(Integer.MAX_VALUE);
		Mockito.when(text.getConverterForCustomer(mockCustomer)).thenReturn(null);
		Mockito.when(text.getValidator()).thenReturn(null);

		// TextFormat with uppercase → "myvalue" becomes "MYVALUE" after toDisplayValue
		TextFormat textFormat = new TextFormat();
		textFormat.setCase(Format.TextCase.upper);
		Mockito.when(text.getFormat()).thenReturn(textFormat);

		// DynamicBean with property "myField" = "myvalue"
		Map<String, Object> props = new HashMap<>();
		props.put("myField", "myvalue");
		DynamicBean dynamicBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, dynamicBean, e);

		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanPropertyTextWithInvalidMaskAddsValidationMessage() {
		// Covers validateFormat L300-303: catch block when format parsing fails
		Customer mockCustomer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);

		Text text = Mockito.mock(Text.class);
		Mockito.when(text.getAttributeType()).thenReturn(AttributeType.text);
		Mockito.when(text.getName()).thenReturn("myField");
		Mockito.when(text.isRequired()).thenReturn(Boolean.FALSE);
		Mockito.when(text.getLocalisedDisplayName()).thenReturn("My Field");
		Mockito.when(text.getLength()).thenReturn(Integer.MAX_VALUE);
		Mockito.when(text.getConverterForCustomer(mockCustomer)).thenReturn(null);
		Mockito.when(text.getValidator()).thenReturn(null);

		// TextFormat with digit mask "###" → "abc" can't be formatted → ParseException
		TextFormat textFormat = new TextFormat();
		textFormat.setMask("###");
		Mockito.when(text.getFormat()).thenReturn(textFormat);

		// DynamicBean with property "myField" = "abc" (letters, not digits → mask fails)
		Map<String, Object> props = new HashMap<>();
		props.put("myField", "abc");
		DynamicBean dynamicBean = new DynamicBean("admin", "User", props);

		ValidationException e = new ValidationException();
		ValidationUtil.validateBeanPropertyAgainstAttribute(user, text, dynamicBean, e);

		// Validation message added for format failure
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateBeanAgainstDocumentTraversesInheritanceChain() {
		Customer mockCustomer = Mockito.mock(Customer.class);
		Mockito.when(user.getCustomer()).thenReturn(mockCustomer);

		// base document with no attributes and no further extends
		Document baseDocument = Mockito.mock(Document.class);
		Mockito.when(baseDocument.getAttributes()).thenReturn(Collections.emptyList());
		Mockito.when(baseDocument.getExtends()).thenReturn(null);

		// child document extends base
		Extends ext = new Extends();
		ext.setDocumentName("BaseDocument");

		Module mockModule = Mockito.mock(Module.class);
		Mockito.when(mockModule.getDocument(mockCustomer, "BaseDocument")).thenReturn(baseDocument);
		Mockito.when(mockCustomer.getModule("admin")).thenReturn(mockModule);

		Document document = Mockito.mock(Document.class);
		Mockito.when(document.getAttributes()).thenReturn(Collections.emptyList());
		Mockito.when(document.getExtends()).thenReturn(ext);
		Mockito.when(document.getOwningModuleName()).thenReturn("admin");

		AbstractPersistence persistence = Mockito.mock(AbstractPersistence.class,
				Mockito.withSettings().defaultAnswer(Mockito.CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
		try {
			assertDoesNotThrow(() -> ValidationUtil.validateBeanAgainstDocument(document, bean));
			// Should traverse the inheritance chain without throwing
		}
		finally {
			clearPersistenceThreadLocal();
		}
	}

	private static void clearPersistenceThreadLocal() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			ThreadLocal<AbstractPersistence> tl = (ThreadLocal<AbstractPersistence>) field.get(null);
			tl.remove();
		}
		catch (@SuppressWarnings("unused") Exception ignored) {
			// ignore
		}
	}
}
