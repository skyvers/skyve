package org.skyve.impl.metadata.controller;

import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.report.ReportFormat;

/**
 * The default Skyve behaviour with no customisations.
 */
public class NoCustomisations implements Customisations {
	@Override
	public HorizontalAlignment determineDefaultTextAlignment(String uxui, AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType) || 
				AttributeType.dateTime.equals(attributeType) ||
				AttributeType.time.equals(attributeType) || 
				AttributeType.timestamp.equals(attributeType) ||
				AttributeType.decimal2.equals(attributeType) || 
				AttributeType.decimal5.equals(attributeType) ||
				AttributeType.decimal10.equals(attributeType) || 
				AttributeType.integer.equals(attributeType) ||
				AttributeType.longInteger.equals(attributeType)) {
			return HorizontalAlignment.right;
		}
		if (AttributeType.bool.equals(attributeType) || 
				AttributeType.content.equals(attributeType) || 
				AttributeType.image.equals(attributeType)) {
			return HorizontalAlignment.centre;
		}
		return HorizontalAlignment.left;
	}
	
	@Override
	public Integer determineDefaultColumnWidth(String uxui, AttributeType attributeType) {
		if (AttributeType.date.equals(attributeType)) {
			return Integer.valueOf(110);
		}
		if (AttributeType.dateTime.equals(attributeType)) {
			return Integer.valueOf(130);
		}
		if (AttributeType.time.equals(attributeType)) {
			return Integer.valueOf(80);
		}
		if (AttributeType.timestamp.equals(attributeType)) {
			return Integer.valueOf(140);
		}
		if (AttributeType.bool.equals(attributeType)) {
			return Integer.valueOf(75);
		}

		return null;
	}

	@Override
	public ReportFormat[] listGridExportFormats() {
		return ReportFormat.values(); // ordered as per the report format enum
	}

	@Override
	public void registerCustomExpressions() {
		// No custom expressions
	}
	
	@Override
	public void registerCustomFormatters() {
		// No custom formatters
	}
}
