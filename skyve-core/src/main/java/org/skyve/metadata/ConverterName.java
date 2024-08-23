package org.skyve.metadata;

import static java.util.stream.Collectors.toMap;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

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
import org.skyve.domain.types.converters.decimal.Decimal10TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
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

import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.COMMON_NAMESPACE)
public enum ConverterName {
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
	Decimal10TwoDecimalPlaces(new Decimal10TwoDecimalPlaces()),
	Decimal2DollarsAndCents(new Decimal2DollarsAndCents()),
	Decimal2DollarsAndCentsAbsolute(new Decimal2DollarsAndCentsAbsolute()),
	Decimal2Integer(new Decimal2Integer()),
	Decimal2IntegerPercentage(new Decimal2IntegerPercentage()),
	Decimal2OneDecimalPlace(new Decimal2OneDecimalPlace()),
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

    /**
     * A map keyed on Converter class, with values point to the relevant ConverterName (ie the 
     * inverse of the definitions above).
     */
    private static Map<Class<?>, ConverterName> classToInstanceIndex = Stream.of(ConverterName.values())
                                                                             .collect(toMap(cn -> cn.getConverter()
                                                                                                    .getClass(),
                                                                                     Function.identity()));

	private Converter<?> converter;
	private ConverterName(Converter<?> converter) {
		this.converter = converter;
	}

	public Converter<?> getConverter() {
		return converter;
	}

    /**
     * Convert from the provided Converter's class to the corresponding ConverterName instance.
     * 
     * @param converter
     * @return
     */
    public static ConverterName valueOf(Converter<?> converter) {

        if (converter == null || !classToInstanceIndex.containsKey(converter.getClass())) {

            throw new IllegalArgumentException(converter + " is not catered for");
        }

        return classToInstanceIndex.get(converter.getClass());
    }
}
