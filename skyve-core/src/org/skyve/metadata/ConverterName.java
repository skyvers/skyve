package org.skyve.metadata;

import javax.xml.bind.annotation.XmlType;

import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.date.DD_MMM_YYYY;
import org.skyve.domain.types.converters.date.DD_MM_YYYY;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MMM_YYYY_HH_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH24_MI;
import org.skyve.domain.types.converters.datetime.DD_MM_YYYY_HH_MI;
import org.skyve.domain.types.converters.decimal.Decimal2Integer;
import org.skyve.domain.types.converters.decimal.Decimal2IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal2OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal2TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5Integer;
import org.skyve.domain.types.converters.decimal.Decimal5IntegerPercentage;
import org.skyve.domain.types.converters.decimal.Decimal5TimeDuration;
import org.skyve.domain.types.converters.decimal.Decimal5OneDecimalPlace;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlaces;
import org.skyve.domain.types.converters.decimal.Decimal5TwoDecimalPlacesPercentage;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.domain.types.converters.decimal.currency.Decimal5DollarsAndCents;
import org.skyve.domain.types.converters.integer.SimplePercentage;
import org.skyve.domain.types.converters.time.HH24_MI;
import org.skyve.domain.types.converters.time.HH24_MI_SS;
import org.skyve.domain.types.converters.time.HH_MI;
import org.skyve.domain.types.converters.time.HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MMM_YYYY_HH_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH24_MI_SS;
import org.skyve.domain.types.converters.timestamp.DD_MM_YYYY_HH_MI_SS;
import org.skyve.impl.util.XMLUtil;

@XmlType(namespace = XMLUtil.COMMON_NAMESPACE)
public enum ConverterName {
	DD_MM_YYYY(new DD_MM_YYYY()),
	DD_MMM_YYYY(new DD_MMM_YYYY()),
	DD_MM_YYYY_HH_MI(new DD_MM_YYYY_HH_MI()),
	DD_MM_YYYY_HH24_MI(new DD_MM_YYYY_HH24_MI()),
	DD_MMM_YYYY_HH_MI(new DD_MMM_YYYY_HH_MI()),
	DD_MMM_YYYY_HH24_MI(new DD_MMM_YYYY_HH24_MI()),
	Decimal2DollarsAndCents(new Decimal2DollarsAndCents()),
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
	HH_MI(new HH_MI()),
	HH24_MI(new HH24_MI()),
	HH_MI_SS(new HH_MI_SS()),
	HH24_MI_SS(new HH24_MI_SS()),
	DD_MM_YYYY_HH_MI_SS(new DD_MM_YYYY_HH_MI_SS()),
	DD_MM_YYYY_HH24_MI_SS(new DD_MM_YYYY_HH24_MI_SS()),
	DD_MMM_YYYY_HH_MI_SS(new DD_MMM_YYYY_HH_MI_SS()),
	DD_MMM_YYYY_HH24_MI_SS(new DD_MMM_YYYY_HH24_MI_SS());
	
	private Converter<?> converter;
	private ConverterName(Converter<?> converter) {
		this.converter = converter;
	}
	
	public Converter<?> getConverter() {
		return converter;
	}
}
