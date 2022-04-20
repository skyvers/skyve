package org.skyve.metadata;

import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.date.DD_MM_YYYY;
import org.skyve.domain.types.converters.date.MMM_DD_YYYY;
import org.skyve.domain.types.converters.date.MM_DD_YYYY;
import org.skyve.domain.types.converters.date.YYYY_MM_DD;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.MMM_DD_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.MM_DD_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH24_MI;
import org.skyve.domain.types.converters.datetime.YYYY_MM_DD_HH_MI;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal2TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5Integer;
import org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5TimeDuration;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCentsAbsolute;
import org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.domain.types.converters.integer.IntegerSeparator;
import org.skyve.domain.types.converters.integer.LongIntegerSeparator;
import org.skyve.domain.types.converters.integer.SimplePercentage;
import org.skyve.domain.types.converters.time.HH24_MI;
import org.skyve.domain.types.converters.time.HH24_MI_SS;
import org.skyve.domain.types.converters.time.HH_MI;
import org.skyve.domain.types.converters.time.HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.MM_DD_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.YYYY_MM_DD_HH_MI_SS;
import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum ConverterName {
	//@formatter:off
	DD_MM_YYYY(new DD_MM_YYYY()),
	DD_MMM_YYYY(new DD_MMM_YYYY()),
	MM_DD_YYYY(new MM_DD_YYYY()),
	MMM_DD_YYYY(new MMM_DD_YYYY()),
	YYYY_MM_DD(new YYYY_MM_DD()),
	DD_MM_YYYY_HH_MI(new DD_MM_YYYY_HH_MI()),
	DD_MM_YYYY_HH24_MI(new DD_MM_YYYY_HH24_MI()),
	DD_MM_YYYY_DateTime(new org.skyve.domain.types.converters.datetime.DD_MM_YYYY()),
	DD_MMM_YYYY_HH_MI(new DD_MMM_YYYY_HH_MI()),
	DD_MMM_YYYY_HH24_MI(new DD_MMM_YYYY_HH24_MI()),
	DD_MMM_YYYY_DateTime(new org.skyve.domain.types.converters.datetime.DD_MMM_YYYY()),
	MM_DD_YYYY_HH_MI(new MM_DD_YYYY_HH_MI()), 
	MM_DD_YYYY_HH24_MI(new MM_DD_YYYY_HH24_MI()),
	MM_DD_YYYY_DateTime(new org.skyve.domain.types.converters.datetime.MM_DD_YYYY()), 
	MMM_DD_YYYY_HH_MI(new MMM_DD_YYYY_HH_MI()), 
	MMM_DD_YYYY_HH24_MI(new MMM_DD_YYYY_HH24_MI()),
	MMM_DD_YYYY_DateTime(new org.skyve.domain.types.converters.datetime.MMM_DD_YYYY()), 
	YYYY_MM_DD_HH_MI(new YYYY_MM_DD_HH_MI()), 
	YYYY_MM_DD_HH24_MI(new YYYY_MM_DD_HH24_MI()),
	YYYY_MM_DD_DateTime(new org.skyve.domain.types.converters.datetime.YYYY_MM_DD()), 
	Decimal2DollarsAndCents(new Decimal2DollarsAndCents()),
	Decimal2DollarsAndCentsAbsolute(new Decimal2DollarsAndCentsAbsolute()),
	Decimal2Integer(new Decimal2Integer()),
	Decimal2IntegerPercentage(new Decimal2IntegerPercentage()),
	Decimal2OneDecimalPlace(new Decimal2OneDecimalPlace()),
	Decimal2TwoDecimalPlacesPercentage(new Decimal2TwoDecimalPlacesPercentage()),
	Decimal5Integer(new Decimal5Integer()),
	Decimal5IntegerPercentage(new Decimal5IntegerPercentage()),
	Decimal5DollarsAndCents(new Decimal5DollarsAndCents()),
	Decimal5TimeDuration(new Decimal5TimeDuration()),
	Decimal5OneDecimalPlace(new Decimal5OneDecimalPlace()),
	Decimal5TwoDecimalPlaces(new Decimal5TwoDecimalPlaces()),
	Decimal5TwoDecimalPlacesPercentage(new Decimal5TwoDecimalPlacesPercentage()),
	SimplePercentage(new SimplePercentage()),
	IntegerSeparator(new IntegerSeparator()),
	LongIntegerSeparator(new LongIntegerSeparator()),
	HH_MI(new HH_MI()),
	HH24_MI(new HH24_MI()),
	HH_MI_SS(new HH_MI_SS()),
	HH24_MI_SS(new HH24_MI_SS()),
	DD_MM_YYYY_HH_MI_SS(new DD_MM_YYYY_HH_MI_SS()),
	DD_MM_YYYY_HH24_MI_SS(new DD_MM_YYYY_HH24_MI_SS()),
	DD_MM_YYYY_Timestamp(new org.skyve.domain.types.converters.timestamp.DD_MM_YYYY()),
	DD_MMM_YYYY_HH_MI_SS(new DD_MMM_YYYY_HH_MI_SS()),
	DD_MMM_YYYY_HH24_MI_SS(new DD_MMM_YYYY_HH24_MI_SS()),
	DD_MMM_YYYY_Timestamp(new org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY()),
	MM_DD_YYYY_HH_MI_SS(new MM_DD_YYYY_HH_MI_SS()),
	MM_DD_YYYY_HH24_MI_SS(new MM_DD_YYYY_HH24_MI_SS()),
	MM_DD_YYYY_Timestamp(new org.skyve.domain.types.converters.timestamp.MM_DD_YYYY()),
	MMM_DD_YYYY_HH_MI_SS(new MMM_DD_YYYY_HH_MI_SS()),
	MMM_DD_YYYY_HH24_MI_SS(new MMM_DD_YYYY_HH24_MI_SS()),
	MMM_DD_YYYY_Timestamp(new org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY()),
	YYYY_MM_DD_HH_MI_SS(new YYYY_MM_DD_HH_MI_SS()),
	YYYY_MM_DD_HH24_MI_SS(new YYYY_MM_DD_HH24_MI_SS()),
	YYYY_MM_DD_Timestamp(new org.skyve.domain.types.converters.timestamp.YYYY_MM_DD());
	//@formatter:on
	
	private Converter<?> converter;
	private ConverterName(Converter<?> converter) {
		this.converter = converter;
	}
	
	public Converter<?> getConverter() {
		return converter;
	}
	
	public static ConverterName valueOf(Converter<?> converter) {
		if (converter instanceof DD_MM_YYYY) {
			return DD_MM_YYYY;
		}
		else if (converter instanceof DD_MMM_YYYY) {
			return DD_MMM_YYYY;
		}
		else if (converter instanceof MM_DD_YYYY) {
			return MM_DD_YYYY;
		}
		else if (converter instanceof MMM_DD_YYYY) {
			return MMM_DD_YYYY;
		}
		else if (converter instanceof DD_MM_YYYY_HH_MI) {
			return DD_MM_YYYY_HH_MI;
		}
		else if (converter instanceof DD_MM_YYYY_HH24_MI) {
			return DD_MM_YYYY_HH24_MI;
		}
		else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MM_YYYY) {
			return DD_MM_YYYY_DateTime;
		}
		else if (converter instanceof DD_MMM_YYYY_HH_MI) {
			return DD_MMM_YYYY_HH_MI;
		}
		else if (converter instanceof DD_MMM_YYYY_HH24_MI) {
			return DD_MMM_YYYY_HH24_MI;
		}
		else if (converter instanceof org.skyve.domain.types.converters.datetime.DD_MMM_YYYY) {
			return DD_MMM_YYYY_DateTime;
		}
		else if (converter instanceof MM_DD_YYYY_HH_MI) {
			return MM_DD_YYYY_HH_MI;
		}
		else if (converter instanceof MM_DD_YYYY_HH24_MI) {
			return MM_DD_YYYY_HH24_MI;
		}
		else if (converter instanceof org.skyve.domain.types.converters.datetime.MM_DD_YYYY) {
			return MM_DD_YYYY_DateTime;
		}
		else if (converter instanceof MMM_DD_YYYY_HH_MI) {
			return MMM_DD_YYYY_HH_MI;
		}
		else if (converter instanceof MMM_DD_YYYY_HH24_MI) {
			return MMM_DD_YYYY_HH24_MI;
		}
		else if (converter instanceof org.skyve.domain.types.converters.datetime.MMM_DD_YYYY) {
			return MMM_DD_YYYY_DateTime;
		}
		else if (converter instanceof Decimal2DollarsAndCents) {
			return Decimal2DollarsAndCents;
		}
		else if (converter instanceof Decimal2DollarsAndCentsAbsolute) {
			return Decimal2DollarsAndCentsAbsolute;
		}
		else if (converter instanceof Decimal2Integer) {
			return Decimal2Integer;
		}
		else if (converter instanceof Decimal2IntegerPercentage) {
			return Decimal2IntegerPercentage;
		}
		else if (converter instanceof Decimal2OneDecimalPlace) {
			return Decimal2OneDecimalPlace;
		}
		else if (converter instanceof Decimal2TwoDecimalPlacesPercentage) {
			return Decimal2TwoDecimalPlacesPercentage;
		}
		else if (converter instanceof Decimal5Integer) {
			return Decimal5Integer;
		}
		else if (converter instanceof Decimal5IntegerPercentage) {
			return Decimal5IntegerPercentage;
		}
		else if (converter instanceof Decimal5DollarsAndCents) {
			return Decimal5DollarsAndCents;
		}
		else if (converter instanceof Decimal5TimeDuration) {
			return Decimal5TimeDuration;
		}
		else if (converter instanceof Decimal5OneDecimalPlace) {
			return Decimal5OneDecimalPlace;
		}
		else if (converter instanceof Decimal5TwoDecimalPlaces) {
			return Decimal5TwoDecimalPlaces;
		}
		else if (converter instanceof Decimal5TwoDecimalPlacesPercentage) {
			return Decimal5TwoDecimalPlacesPercentage;
		}
		else if (converter instanceof SimplePercentage) {
			return SimplePercentage;
		}
		else if (converter instanceof IntegerSeparator) {
			return IntegerSeparator;
		}
		else if (converter instanceof LongIntegerSeparator) {
			return LongIntegerSeparator;
		}
		else if (converter instanceof HH_MI) {
			return HH_MI;
		}
		else if (converter instanceof HH24_MI) {
			return HH24_MI;
		}
		else if (converter instanceof HH_MI_SS) {
			return HH_MI_SS;
		}
		else if (converter instanceof HH24_MI_SS) {
			return HH24_MI_SS;
		}
		else if (converter instanceof DD_MM_YYYY_HH_MI_SS) {
			return DD_MM_YYYY_HH_MI_SS;
		}
		else if (converter instanceof DD_MM_YYYY_HH24_MI_SS) {
			return DD_MM_YYYY_HH24_MI_SS;
		}
		else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MM_YYYY) {
			return DD_MM_YYYY_Timestamp;
		}
		else if (converter instanceof DD_MMM_YYYY_HH_MI_SS) {
			return DD_MMM_YYYY_HH_MI_SS;
		}
		else if (converter instanceof DD_MMM_YYYY_HH24_MI_SS) {
			return DD_MMM_YYYY_HH24_MI_SS;
		}
		else if (converter instanceof org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY) {
			return DD_MMM_YYYY_Timestamp;
		}
		else if (converter instanceof MM_DD_YYYY_HH_MI_SS) {
			return MM_DD_YYYY_HH_MI_SS;
		}
		else if (converter instanceof MM_DD_YYYY_HH24_MI_SS) {
			return MM_DD_YYYY_HH24_MI_SS;
		}
		else if (converter instanceof org.skyve.domain.types.converters.timestamp.MM_DD_YYYY) {
			return MM_DD_YYYY_Timestamp;
		}
		else if (converter instanceof MMM_DD_YYYY_HH_MI_SS) {
			return MMM_DD_YYYY_HH_MI_SS;
		}
		else if (converter instanceof MMM_DD_YYYY_HH24_MI_SS) {
			return MMM_DD_YYYY_HH24_MI_SS;
		}
		else if (converter instanceof org.skyve.domain.types.converters.timestamp.MMM_DD_YYYY) {
			return MMM_DD_YYYY_Timestamp;
		}
		else {
			throw new IllegalArgumentException(converter + " is not catered for");
		}
	}
}
