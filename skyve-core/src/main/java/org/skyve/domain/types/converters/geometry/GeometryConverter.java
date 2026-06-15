package org.skyve.domain.types.converters.geometry;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.io.WKTWriter;
import org.skyve.domain.messages.ConversionException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.domain.types.converters.Validator;
import org.skyve.metadata.model.Attribute.AttributeType;

/**
 * Converts geometry values to and from Well-Known Text (WKT).
 */
public class GeometryConverter implements Converter<Geometry> {
	/**
	 * Returns the value type handled by this converter.
	 * @return the result value
	 */
	@Override
	public Class<Geometry> getValueType() {
		return Geometry.class;
	}
	
	/**
	 * Converts a display representation to its domain value.
	 * @param displayValue the display value
	 * @return the result value
	 */
	@Override
	public Geometry fromDisplayValue(String displayValue) throws ConversionException {
		try {
			return new WKTReader().read(displayValue);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.GEOMETRY_CONVERTER_KEY, e);
		}
	}

	/**
	 * Converts a domain value to its display representation.
	 * @param value the value
	 * @return the result value
	 */
	@Override
	public String toDisplayValue(Geometry value) throws ConversionException {
		try {
			return new WKTWriter().write(value);
		}
		catch (Exception e) {
			throw new ConversionException(ConversionException.GEOMETRY_CONVERTER_KEY, e);
		}
	}

	/**
	 * Returns the attribute type supported by this converter.
	 * @return the result value
	 */
	@Override
	public AttributeType getAttributeType() {
		return AttributeType.geometry;
	}

	/**
	 * Returns the optional format descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Format<Geometry> getFormat() {
		return null;
	}

	/**
	 * Returns the optional validator descriptor for this converter.
	 * @return the result value
	 */
	@Override
	public Validator<Geometry> getValidator() {
		return null;
	}
	
	/**
	 * Returns the optional format pattern for this converter.
	 * @return the result value
	 */
	@Override
	public String getFormatPattern() {
		return null;
	}
}
