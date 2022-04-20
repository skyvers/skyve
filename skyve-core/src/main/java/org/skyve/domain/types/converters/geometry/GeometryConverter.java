package org.skyve.domain.types.converters.geometry;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

public class GeometryConverter implements Converter<Geometry> {
	@Override
	public Geometry fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new WKTReader().read(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.GEOMETRY_CONVERTER_KEY, e);
		}
	}

	@Override
	public String toDisplayValue(Geometry value) throws ConversionException {
		try {
			return new WKTWriter().write(value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.GEOMETRY_CONVERTER_KEY, e);
		}
	}

	@Override
	public AttributeType getAttributeType() {
		return AttributeType.geometry;
	}

	@Override
	public Format<Geometry> getFormat() {
		return null;
	}

	@Override
	public Validator<Geometry> getValidator() {
		return null;
	}
}
