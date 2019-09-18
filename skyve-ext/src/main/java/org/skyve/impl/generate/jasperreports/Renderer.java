package org.skyve.impl.generate.jasperreports;

import java.io.File;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.types.converters.decimal.currency.Decimal2DollarsAndCents;
import org.skyve.impl.generate.jasperreports.DesignSpecification.Mode;
import org.skyve.impl.generate.jasperreports.DesignSpecification.ReportType;
import org.skyve.impl.generate.jasperreports.ReportBand.BandType;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;

public class Renderer {

	public static final int defaultReportWith = 842;
	public static final int defaultReportHeight = 595;

	public static String eS(String string, Map<String, String> attributes, boolean withTerminator) {
		StringBuilder sb = new StringBuilder();
		sb.append("\n");
		sb.append("<").append(string);
		if (attributes != null) {
			for (String k : attributes.keySet()) {
				sb.append(" ").append(k).append("=");
				sb.append("\"").append(attributes.get(k)).append("\"");
			}
		}
		if (withTerminator) {
			sb.append("/>");
		} else {
			sb.append(">");
		}
		return sb.toString();
	}

	public static String eS(String string, String key, String value, boolean withTerminator) {
		StringBuilder sb = new StringBuilder();
		sb.append("\n");
		sb.append("<").append(string);
		if (key != null) {
			sb.append(" ").append(key).append("=");
			sb.append("\"").append(value).append("\"");
		}
		if (withTerminator) {
			sb.append("/>");
		} else {
			sb.append(">");
		}
		return sb.toString();
	}

	public static String eS(String string, String key, String value) {
		return eS(string, key, value, true);
	}

	public static String eS(String string, String key1, String value1, String key2, String value2) {
		StringBuilder sb = new StringBuilder();
		sb.append("\n");
		sb.append("<").append(string);
		if (key1 != null) {
			sb.append(" ").append(key1).append("=");
			sb.append("\"").append(value1).append("\"");
		}
		if (key2 != null) {
			sb.append(" ").append(key2).append("=");
			sb.append("\"").append(value2).append("\"");
		}
		sb.append("/>");
		return sb.toString();
	}

	public static String eS(String string, String key1, String value1, String key2, String value2, String key3, String value3) {
		StringBuilder sb = new StringBuilder();
		sb.append("\n");
		sb.append("<").append(string);
		if (key1 != null) {
			sb.append(" ").append(key1).append("=");
			sb.append("\"").append(value1).append("\"");
		}
		if (key2 != null) {
			sb.append(" ").append(key2).append("=");
			sb.append("\"").append(value2).append("\"");
		}
		if (key3 != null) {
			sb.append(" ").append(key3).append("=");
			sb.append("\"").append(value3).append("\"");
		}
		sb.append("/>");
		return sb.toString();
	}

	public static String eF(String string) {
		return "\n</" + string + ">";
	}

	public static String renderElement(ReportElement elem) {

		StringBuilder sb = new StringBuilder();

		if (ElementType.textField.equals(elem.getElementType())) {
			Map<String, String> attr = new LinkedHashMap<>();
			if (Boolean.TRUE.equals(elem.getDynamicFlow())) {
				attr.put("isStretchWithOverflow", "true");
			}
			if (elem.getEvaluationTime() != null) {
				attr.put("evaluationTime", elem.getEvaluationTime().toString());
			}
			attr.put("pattern", "");
			attr.put("isBlankWhenNull", "true");
			sb.append(Renderer.eS(elem.getElementType().toString(), attr, false));
		} else if (ElementType.staticText.equals(elem.getElementType())) {
			sb.append("<staticText>");
		}

		switch (elem.getElementType()) {
		case staticText:
		case textField:

			Map<String, String> rEa = new LinkedHashMap<>();
			rEa.put("key", elem.getElementType().toString() + "_" + (elem.getOrdinal() == null ? "1" : elem.getOrdinal()));
			rEa.put("stretchType", "RelativeToTallestObject");
			if (elem.getElementForeColour() != null) {
				rEa.put("forecolor", elem.getElementForeColour());
			} else {
				rEa.put("forecolor", "#404040");
			}
			if (elem.getElementBackColour() != null) {
				rEa.put("mode", "Opaque");
				rEa.put("backcolor", elem.getElementBackColour());
			} else {
				rEa.put("mode", "Transparent");
				rEa.put("backcolor", "#FFFFFF");
			}

			rEa.put("x", (elem.getElementLeft() == null ? "0" : elem.getElementLeft().toString()));
			rEa.put("y", (elem.getElementTop() == null ? "0" : elem.getElementTop().toString()));
			rEa.put("height", (elem.getElementHeight() == null ? "0" : elem.getElementHeight().toString()));
			rEa.put("width", (elem.getElementWidth() == null ? "0" : elem.getElementWidth().toString()));
			rEa.put("uuid", UUID.randomUUID().toString());
			sb.append(Renderer.eS("reportElement", rEa, false));
			sb.append(renderPrintWhenExpression(elem.getParent().getParent(), elem.getInvisibleConditionName()));
			sb.append("</reportElement>");

			try {
				sb.append(renderBox(elem));
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			Map<String, String> tEa = new LinkedHashMap<>();
			if (elem.getElementAlignment() != null) {
				tEa.put("textAlignment", elem.getElementAlignment().toString());
			}
			tEa.put("verticalAlignment", "Top");
			tEa.put("rotation", "None");
			sb.append(Renderer.eS("textElement", tEa, false));

			Map<String, String> fEa = new LinkedHashMap<>();
			fEa.put("fontName", elem.getElementFontName());
			if (BandType.title.equals(elem.getParent().getBandType())) {
				if (elem.getParent().getParent().getTitleFontSize() != null) {
					fEa.put("size", elem.getParent().getParent().getTitleFontSize().toString());
				} else {
					fEa.put("size", "16");
				}
				fEa.put("isBold", (Boolean.TRUE.equals(elem.getElementBold()) ? "true" : "false"));
				fEa.put("isItalic", (Boolean.TRUE.equals(elem.getElementItalic()) ? "true" : "false"));
				fEa.put("isUnderline", "false");
				fEa.put("isStrikeThrough", "false");
				// fEa.put("pdfFontName", "Helvetica-Bold");
				// fEa.put("pdfEncoding", "Cp1250");
				// fEa.put("isPdfEmbedded", "false");

			} else {
				if (elem.getElementFontSize() != null) {
					fEa.put("size", elem.getElementFontSize().toString());
				} else {
					fEa.put("size", "12");
				}
				fEa.put("isBold", (Boolean.TRUE.equals(elem.getElementBold()) ? "true" : "false"));
				fEa.put("isItalic", (Boolean.TRUE.equals(elem.getElementItalic()) ? "true" : "false"));
				fEa.put("isUnderline", "false");
				fEa.put("isStrikeThrough", "false");
				// fEa.put("pdfFontName", "Helvetica");
				// fEa.put("pdfEncoding", "Cp1250");
				// fEa.put("isPdfEmbedded", "false");
			}
			sb.append(Renderer.eS("font", fEa, true));

			sb.append(Renderer.eF("textElement"));

			if (ElementType.staticText.equals(elem.getElementType())) {
				sb.append(Renderer.eS("text", null, false));
			} else {
				sb.append(Renderer.eS("textFieldExpression", null, false));
			}
			sb.append("<![CDATA[");
			if (ElementType.textField.equals(elem.getElementType())) {
				if (elem.getElementValue() == null) {
					sb.append("\"\"");
				} else {
					sb.append(elem.getElementValue());
				}
			} else {
				sb.append(elem.getElementValue());
			}
			sb.append("]]>");
			if (ElementType.staticText.equals(elem.getElementType())) {
				sb.append(Renderer.eF("text"));
			} else {
				sb.append(Renderer.eF("textFieldExpression"));
			}
			sb.append(Renderer.eF(elem.getElementType().toString()));

			break;
		case contentImage:
		case staticImage:
		case dynamicImage:
			// TODO
			// <image>
			// <reportElement x="354" y="14" width="100" height="50" uuid="3aec0b08-3306-46cb-a84a-bb500a34474e"/>
			// </image>
			StringBuilder iE = new StringBuilder(64);
			if (ElementType.dynamicImage.equals(elem.getElementType())) {
				iE.append("new modules").append('.');
				iE.append(elem.getParent().getParent().getModuleName()).append('.');
				iE.append(elem.getParent().getParent().getDocumentName()).append('.');
				iE.append("images").append('.');
				iE.append(elem.getElementValue()).append("().getImage(");
				if (Mode.bean.equals(elem.getParent().getParent().getMode())) {
					iE.append("$F{THIS}, ");
				} else {
					// sql
					iE.append("org.skyve.impl.generate.jasperreports.BeanForReport.getBean(");
					iE.append(elem.getParent().getParent().getModuleName()).append(", ");
					iE.append(elem.getParent().getParent().getDocumentName()).append(", ");
					iE.append("$P{ID})");
				}
				if (elem.getElementWidth() != null) {
					iE.append(elem.getElementWidth().toString()).append(", ");
					iE.append(elem.getElementWidth().toString()).append(", ");
				}
				// TODO vertical sizing - for now assume square based on pixelWidth
				iE.append(" (org.skyve.metadata.user.User) org.skyve.impl.generate.jasperreports.BeanForReport.getUser())");
			} else if (ElementType.staticImage.equals(elem.getElementType())) {
				iE.append(elem.getElementValue());
			} else if (ElementType.contentImage.equals(elem.getElementType())) {
				iE.append("org.skyve.impl.generate.jasperreports.ContentImageForReport.image(");
				iE.append("$F{").append(elem.getElementValue()).append("}, ");

				if (elem.getElementWidth() != null) {
					iE.append(elem.getElementWidth().toString()).append(", ");
					iE.append(elem.getElementWidth().toString());
				}
				iE.append(")");
			}

			sb.append("<image>");
			Map<String, String> rEIm = new LinkedHashMap<>();
			rEIm.put("x", elem.getElementLeft().toString());
			rEIm.put("y", elem.getElementTop().toString());
			rEIm.put("width", elem.getElementWidth().toString());
			rEIm.put("height", elem.getElementHeight().toString());
			rEIm.put("uuid", UUID.randomUUID().toString());

			sb.append(Renderer.eS("reportElement", rEIm, true));

			try {
				sb.append(renderBox(elem));
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			sb.append("<imageExpression><![CDATA[");
			sb.append(iE.toString());
			sb.append("]]></imageExpression>");

			sb.append("</image>");
			break;
		case line:

			Map<String, String> rEaL = new LinkedHashMap<>();
			rEaL.put("key", elem.getElementType().toString() + elem.getOrdinal());
			rEaL.put("x", elem.getElementLeft().toString());
			rEaL.put("y", elem.getElementTop().toString());
			rEaL.put("width", elem.getElementWidth().toString());
			rEaL.put("height", elem.getElementHeight().toString());
			rEaL.put("forecolor", "#404040");
			rEaL.put("uuid", UUID.randomUUID().toString());
			sb.append(Renderer.eS("reportElement", rEaL, true));

			sb.append(Renderer.eS("graphicElement", null, false));

			Map<String, String> tEaL = new LinkedHashMap<>();
			if (elem.getParent().getParent().getDefaultLineWidth() != null) {
				tEaL.put("lineStyle", "Solid");
				tEaL.put("lineWidth", elem.getParent().getParent().getDefaultLineWidth().toString());
			} else {
				tEaL.put("lineStyle", "Solid");
				tEaL.put("lineWidth", "1.0");
			}
			sb.append(Renderer.eS("pen", tEaL, false));

			sb.append(Renderer.eF("graphicElement"));
			sb.append(Renderer.eF(elem.getElementType().toString()));

			break;
		case border:

			sb.append("<rectangle>");

			Map<String, String> rEB = new LinkedHashMap<>();

			rEB.put("stretchType", "RelativeToBandHeight");
			rEB.put("x", elem.getElementLeft().toString());
			rEB.put("y", elem.getElementTop().toString());
			rEB.put("width", elem.getElementWidth().toString());
			rEB.put("height", elem.getElementHeight().toString());
			if (elem.getParent().getParent().getDefaultLineColour() != null) {
				rEB.put("forecolor", elem.getParent().getParent().getDefaultLineColour());
			}
			rEB.put("uuid", UUID.randomUUID().toString());
			sb.append(Renderer.eS("reportElement", rEB, true));

			if (elem.getParent().getParent().getDefaultLineWidth() != null) {
				sb.append(Renderer.eS("graphicElement", null, false));
				sb.append(Renderer.eS("pen", "lineWidth", elem.getParent().getParent().getDefaultLineWidth().toString(), "lineStyle", "Solid"));
				sb.append(Renderer.eF("graphicElement"));
			}

			sb.append("</rectangle>");

			break;

		case subreport:

			sb.append("<").append(elem.getElementType().name()).append(">");

			Map<String, String> rSr = new LinkedHashMap<>();
			rSr.put("x", elem.getElementLeft().toString());
			rSr.put("y", elem.getElementTop().toString());
			rSr.put("width", elem.getElementWidth().toString());
			rSr.put("height", elem.getElementHeight().toString());
			rSr.put("uuid", UUID.randomUUID().toString());
			sb.append(Renderer.eS("reportElement", rSr, true));

			if (Mode.bean.equals(elem.getParent().getParent().getMode())) {
				sb.append(eS("dataSourceExpression", null, false));
				sb.append("<![CDATA[new net.sf.jasperreports.engine.data.JRBeanCollectionDataSource($F{");
				sb.append(elem.getName());
				sb.append("})]]>");
				sb.append(eF("dataSourceExpression"));
			} else {
				sb.append(eS("subreportParameter", "name", "ID", false));
				sb.append(eS("subreportParameterExpression", null, false));
				sb.append("<![CDATA[$P{ID}]]>");
				sb.append(eF("subreportParameterExpression"));
				sb.append(eF("subreportParameter"));

				sb.append(eS("connectionExpression", null, false));
				sb.append("<![CDATA[$P{REPORT_CONNECTION}]]>");
				sb.append(eF("connectionExpression"));
			}

			sb.append(eS("subreportExpression", null, false));
			sb.append("<![CDATA[$P{SUBREPORT_DIR} + \"");
			sb.append(elem.getReportFileName());
			sb.append(".jasper\"]]>");
			sb.append(eF("subreportExpression"));

			sb.append(Renderer.eF(elem.getElementType().toString()));
			break;
		default:
			break;

		}

		return sb.toString();
	}

	public static String renderBox(ReportElement e) throws Exception {
		StringBuilder sb = new StringBuilder();

		Map<String, String> a = new LinkedHashMap<>();
		if (e.getTopPadding() != null) {
			a.put("topPadding", e.getTopPadding().toString());
		}
		if (e.getLeftPadding() != null) {
			a.put("leftPadding", e.getLeftPadding().toString());
		}
		if (e.getBottomPadding() != null) {
			a.put("bottomPadding", e.getBottomPadding().toString());
		}
		if (e.getRightPadding() != null) {
			a.put("rightPadding", e.getRightPadding().toString());
		}

		sb.append(Renderer.eS("box", a, false));
		Decimal2DollarsAndCents conv = new Decimal2DollarsAndCents();
		String lineWidth = "1";
		if (e.getBorderLineWidth() != null) {
			lineWidth = conv.toDisplayValue(e.getBorderLineWidth());
		}
		if (Boolean.TRUE.equals(e.getElementBorder())) {
			if (e.getBorderLineWidth() != null) {
				sb.append(Renderer.eS("pen", "lineWidth", lineWidth, "lineColor", e.getBorderColour()));
			}
			if (Boolean.TRUE.equals(e.getBorderTop())) {
				sb.append(Renderer.eS("topPen", "lineStyle", "Solid", "lineWidth", lineWidth, "lineColor", e.getBorderColour()));
			}
			if (Boolean.TRUE.equals(e.getBorderLeft())) {
				sb.append(Renderer.eS("leftPen", "lineStyle", "Solid", "lineWidth", lineWidth, "lineColor", e.getBorderColour()));
			}
			if (Boolean.TRUE.equals(e.getBorderBottom())) {
				sb.append(Renderer.eS("bottomPen", "lineStyle", "Solid", "lineWidth", lineWidth, "lineColor", e.getBorderColour()));
			}
			if (Boolean.TRUE.equals(e.getBorderTop())) {
				sb.append(Renderer.eS("rightPen", "lineStyle", "Solid", "lineWidth", lineWidth, "lineColor", e.getBorderColour()));
			}
		} else {
			sb.append(Renderer.eS("topPen", "lineStyle", "Solid"));
			sb.append(Renderer.eS("leftPen", "lineStyle", "Solid"));
			sb.append(Renderer.eS("bottomPen", "lineStyle", "Solid"));
			sb.append(Renderer.eS("rightPen", "lineStyle", "Solid"));
		}
		sb.append(Renderer.eF("box"));

		return sb.toString();
	}

	public static String renderBand(ReportBand band) {

		StringBuilder sb = new StringBuilder();

		// don't do report section for detail
		if (!BandType.detail.equals(band.getBandType())) {
			sb.append(eS(band.getBandType().toString(), null, false));
		}

		if (band.getElements().isEmpty()) {
			sb.append(eS("band", null, true));
		} else {

			Map<String, String> bandAttr = new HashMap<>();
			bandAttr.put("height", band.getHeight().toString());
			if (band.getSplitType() != null) {
				bandAttr.put("splitType", band.getSplitType().toString());
			}

			sb.append(eS("band", bandAttr, false));
			if (band.getInvisibleConditionName() != null) {
				sb.append(renderPrintWhenExpression(band.getParent(), band.getInvisibleConditionName()));
			}

			for (ReportElement e : band.getElements()) {
				sb.append(e.getJrxml());
			}

			sb.append(eF("band"));
		}

		if (!BandType.detail.equals(band.getBandType())) {
			sb.append(eF(band.getBandType().toString()));
		}

		return sb.toString();
	}

	public static String renderField(ReportField field) {

		StringBuilder sb = new StringBuilder();

		if (!(Boolean.TRUE.equals(field.getCollection()) && Mode.sql.equals(field.getParent().getMode()))) {
			if (field.getParent() != null) {
				if (Mode.sql.equals(field.getParent().getMode())) {
					Map<String, String> attr = new LinkedHashMap<>();
					attr.put("name", field.getName());
					attr.put("class", field.getTypeClass());

					sb.append(eS("field", attr, true));
				} else {
					// bean mode relies on fieldDescription using Binder result
					Map<String, String> attr = new LinkedHashMap<>();
					attr.put("name", field.getName());
					attr.put("class", field.getTypeClass());

					sb.append(eS("field", attr, false));
					if ("java.lang.String".equals(field.getTypeClass())) {
						sb.append(eS("fieldDescription", null, false));
						sb.append("<![CDATA[").append(field.getName()).append("]]>");
						sb.append(eF("fieldDescription"));
					}
					sb.append(eF("field"));
				}
			}
		}

		return sb.toString();
	}

	public static String renderParameter(ReportParameter param) {

		StringBuilder sb = new StringBuilder();

		Map<String, String> attr = new LinkedHashMap<>();
		attr.put("name", param.getName());
		attr.put("class", param.getTypeClass());
		attr.put("isForPrompting", "false");

		sb.append(eS("parameter", attr, false));
		sb.append(eS("defaultValueExpression", null, false));
		sb.append("<![CDATA[").append((param.getDefaultValueExpression() == null ? "" : param.getDefaultValueExpression())).append("]]>");
		sb.append(eF("defaultValueExpression"));
		sb.append(eF("parameter"));

		return sb.toString();
	}

	public static String renderVariable(ReportVariable variable) {

		StringBuilder sb = new StringBuilder();

		Map<String, String> attr = new LinkedHashMap<>();
		attr.put("name", variable.getName());
		attr.put("class", variable.getTypeClass());
		attr.put("incrementType", "Column");
		// attr.put("calculation", "None");

		sb.append(eS("variable", attr, false));

		sb.append(eS("variableExpression", null, false));
		sb.append("<![CDATA[$V{").append(variable.getName()).append("}.add($F{").append(variable.getName()).append("})]]>");
		sb.append(eF("variableExpression"));

		sb.append(eS("initialValueExpression", null, false));
		sb.append("<![CDATA[new ").append(variable.getTypeClass()).append("(0)]]>");
		sb.append(eF("initialValueExpression"));

		sb.append(eF("variable"));

		return sb.toString();
	}

	public static String renderDesign(DesignSpecification design) {

		StringBuilder sb = new StringBuilder();

		if (design.getModuleName() != null && design.getDocumentName() != null) {

			sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

			// reportHeader
			Map<String, String> attr = new LinkedHashMap<>();
			attr.put("xmlns", "http://jasperreports.sourceforge.net/jasperreports");
			attr.put("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
			attr.put("xsi:schemaLocation", "http://jasperreports.sourceforge.net/jasperreports http://jasperreports.sourceforge.net/xsd/jasperreport.xsd");
			attr.put("name", design.getName());
			attr.put("pageWidth", design.getWidth().toString());
			attr.put("pageHeight", design.getHeight().toString());
			// attr.put("orientation", design.getOrientation().toCode());
			attr.put("columnWidth", design.getColumnWidth().toString());
			attr.put("leftMargin", design.getLeftMargin().toString());
			attr.put("rightMargin", design.getRightMargin().toString());
			attr.put("topMargin", design.getTopMargin().toString());
			attr.put("bottomMargin", design.getBottomMargin().toString());
			attr.put("uuid", UUID.randomUUID().toString());

			sb.append(eS("jasperReport", attr, false));

			// properties
			sb.append(eS("property", "name", "ireport.scriptlethandling", "value", "0"));
			sb.append(eS("property", "name", "ireport.encoding", "value", "UTF-8"));
			sb.append(eS("property", "name", "ireport.zoom", "value", "1.0"));
			sb.append(eS("property", "name", "ireport.x", "value", "0"));
			sb.append(eS("property", "name", "ireport.y", "value", "0"));

			// imports
			sb.append(eS("import", "value", "net.sf.jasperreports.engine.*"));
			sb.append(eS("import", "value", "java.util.*"));
			sb.append(eS("import", "value", "net.sf.jasperreports.engine.data.*"));

			// parameters
			for (ReportParameter param : design.getParameters()) {
				sb.append(param.getJrxml());
			}

			// get persistent name for document
			Customer customer = CORE.getPersistence().getUser().getCustomer();
			Module module = customer.getModule(design.getModuleName());
			Document document = module.getDocument(customer, design.getDocumentName());

			// queryString
			if (Mode.bean.equals(design.getMode())) {
				sb.append(eS("queryString", "language", "document", false));
			} else {
				sb.append(eS("queryString", null, false));
			}
			sb.append("<![CDATA[");

			if (Mode.bean.equals(design.getMode())) {
				if (ReportType.report.equals(design.getReportType())) {
					sb.append(design.getModuleName()).append('.').append(design.getDocumentName());
				} else {
					// nothing
				}
			} else {
				String sqlName = null;
				if (document.getExtends() != null) {
					Document extDocument = null;
					if (document.getExtends().getDocumentName() != null && ExtensionStrategy.joined.equals(document.getPersistent().getStrategy())) {
						extDocument = module.getDocument(customer, document.getExtends().getDocumentName());
						design.setAlias(design.getAlias() + 1);
					}
					sqlName = extDocument.getName();
				}

				StringBuilder sql = new StringBuilder();
				for (ReportField f : design.getFields()) {
					if (!Boolean.TRUE.equals(f.getCollection())) {
						if (sql.length() > 0) {
							sql.append("\n ,");
						}
						sql.append((f.getNameSql() == null ? sqlName + "." + f.getName() : f.getNameSql()));
					}
				}

				if (ReportType.report.equals(design.getReportType())) {
					// not implemented
				} else {
					// nothing
				}

				sb.append("select ").append(sql).append(" from ").append(getPersistentFromDocument(document)).append(" a");

				// joins
				if (design.getJoins() != null) {
					for (String k : design.getJoins().keySet()) {
						sb.append("\n").append(design.getJoins().get(k));
					}
				}

				if (ReportType.report.equals(design.getReportType())) {
					sb.append("\n where ").append("a").append(".bizId = $P{ID}");
				} else if (ReportType.subreport.equals(design.getReportType())) {
					Util.LOGGER.info("SUBREPORT " + design.getName() + " IS " + design.getCollectionType().name());

					// join to either parent or joiner table
					if (CollectionType.child.equals(design.getCollectionType())) {
						// child
						sb.append("\n where ").append("a").append(".parent_id = $P{ID}");
					} else {
						// joiner
						// TODO - join correctly for aggregated collections - probably obviated by the getJoins() above
						sb.append("\n join ").append(design.getParentReportPersistentName()).append("_").append(design.getField().getName());
						sb.append(" ").append(design.getField().getName()).append(" on ").append(design.getField().getName()).append(".element_id = ");
						sb.append(" ").append(document.getName()).append(".bizId");
						sb.append("\n where ").append(design.getField().getName()).append(".owner_id = $P{ID}");
					}
				}
			}
			sb.append("]]>");
			sb.append(eF("queryString"));

			// fields
			for (ReportField field : design.getFields()) {
				sb.append(field.getJrxml());
			}

			// variables
			for (ReportVariable variable : design.getVariables()) {
				sb.append(variable.getJrxml());
			}

			// bands
			boolean detailBandStarted = false;
			for (ReportBand band : design.getBands()) {

				if (BandType.detail.equals(band.getBandType())) {
					if (!detailBandStarted) {
						sb.append(eS(BandType.detail.toString(), null, false));
						detailBandStarted = true;
					}
				}

				// close detail before starting another band
				if (!BandType.detail.equals(band.getBandType())) {
					if (detailBandStarted) {
						sb.append(eF(BandType.detail.toString()));
						detailBandStarted = false;
					}
				}

				sb.append(band.getJrxml());

			}

			sb.append(eF("jasperReport"));
		}

		return sb.toString();
	}

	/**
	 * Skyve provides invisibleConditionName, so printWhenExpression must invert the polarity as well as render as jasper expression
	 * 
	 * @param design
	 * @param invisibleConditionName
	 * @return
	 */
	public static String renderPrintWhenExpression(DesignSpecification design, String invisibleConditionName) {
		if (invisibleConditionName == null || StringUtils.isBlank(invisibleConditionName)) {
			return "";
		}
		StringBuilder sb = new StringBuilder(64);
		sb.append("<printWhenExpression><![CDATA[");
		if (Mode.bean.equals(design.getMode())) {
			sb.append("$F{THIS}.").append(flipCondition(invisibleConditionName)).append("()");
		} else {
			if (!invisibleConditionName.startsWith("not")) {
				sb.append("!");
			}
			sb.append("org.skyve.impl.generate.jasperreports.BeanForReport.evaluateCondition(");
			sb.append("\"").append(design.getModuleName()).append("\",");
			sb.append("\"").append(design.getDocumentName()).append("\",");
			sb.append("$P{ID},");
			sb.append("\"").append(rawConditionName(invisibleConditionName)).append("\"");
			sb.append(")");
		}
		sb.append("]]></printWhenExpression>");
		return sb.toString();
	}

	public static String pathToReport(String moduleName, String documentName, boolean enquote) {

		String sep = System.getProperty("file.separator");

		String basePath = "";
		basePath = UtilImpl.getAbsoluteBasePath();
		if (basePath.startsWith("/")) {
			basePath = basePath.substring(1);
		}
		basePath = basePath.replace("/", sep);

		StringBuilder sPath = new StringBuilder();
		if (enquote) {
			sPath.append("\"");
		}

		sPath.append(basePath);
		sPath.append("src/skyve/");
		sPath.append("modules");
		sPath.append(sep).append(moduleName);
		sPath.append(sep).append(documentName);
		sPath.append(sep).append("reports");
		sPath.append(sep);

		if (enquote) {
			sPath.append("\"");
		}

		if (enquote && "\\".equals(sep)) {
			return sPath.toString().replace(sep, sep + sep);
		}
		return sPath.toString();
	}

	public static String getPersistentFromDocument(Document document) {

		StringBuilder sb = new StringBuilder();

		if (document.getPersistent() != null) {
			sb.append(document.getPersistent().getCatalog() == null ? "" : document.getPersistent().getCatalog() + ".");
			sb.append(document.getPersistent().getSchema() == null ? "" : document.getPersistent().getSchema() + ".");
			sb.append(document.getPersistent().getName());
		}

		return sb.toString();
	}

	public static String getSqlEquivalentClass(AttributeType aType) {

		String result = null;

		switch (aType) {
		case decimal2:
		case decimal5:
		case decimal10:
			result = "java.math.BigDecimal";
			break;
		case integer:
			result = "java.lang.Integer";
			break;
		case longInteger:
			result = "java.lang.Long";
			break;
		case date:
		case dateTime:
		case timestamp:
			result = "java.util.Date";
			break;
		case bool:
			result = "java.lang.Boolean";
			break;
		default:
			result = "java.lang.String";
			break;
		}

		return result;
	}

	/**
	 * Construct a report element and add it to the elements collection within the band
	 * 
	 * @param parent
	 * @param type
	 * @param name
	 * @param valueExpression
	 * @param fontName
	 * @param fontSize
	 * @param top
	 * @param left
	 * @param width
	 * @param height
	 * @return
	 * @throws Exception
	 */
	public static ReportBand addElement(ReportBand band, ReportElement.ElementType type, String name, String valueExpression, String fontName, Integer fontSize, Integer top,
			Integer left,
			Integer width, Integer height, Boolean border, ReportElement.ElementAlignment alignment, Boolean bold, Boolean italic, String foreColour, String backColour,
			String printWhenExpression) {
		try {

			ReportElement tE = new ReportElement(type, name, valueExpression, fontName, fontSize, top, left, width, height, border, alignment, bold, italic, printWhenExpression);

			if (tE != null) {
				tE.setParent(band);

				// inherit from band
				if (tE.getElementFontName() == null) {
					tE.setElementFontName(band.getParent().getDefaultFontName());
				}

				// font size
				if (tE.getElementFontSize() == null) {
					tE.setElementFontSize(band.getParent().getDefaultFontSize());
				}

				// height
				if (tE.getElementHeight() == null) {
					tE.setElementHeight(band.getParent().getDefaultElementHeight());
				}

				// dynamic flow
				if (tE.getDynamicFlow() == null) {
					tE.setDynamicFlow(band.getParent().getDynamicFlow());
				}

				// border
				if (tE.getElementBorder() == null) {
					tE.setElementBorder(band.getParent().getDefaultBorder());
				}
				tE.setBorderLineWidth(band.getParent().getDefaultLineWidth());
				tE.setBorderColour(band.getParent().getDefaultLineColour());

				// colour
				if (foreColour != null) {
					tE.setElementForeColour(foreColour);
				}
				if (backColour != null) {
					tE.setElementBackColour(backColour);
				}

				tE.setBorderTop(band.getParent().getDefaultBorderTop());
				tE.setBorderLeft(band.getParent().getDefaultBorderLeft());
				tE.setBorderBottom(band.getParent().getDefaultBorderBottom());
				tE.setBorderRight(band.getParent().getDefaultBorderRight());

				// padding
				tE.setTopPadding(band.getParent().getDefaultCellTopPadding());
				tE.setLeftPadding(band.getParent().getDefaultCellLeftPadding());
				tE.setBottomPadding(band.getParent().getDefaultCellBottomPadding());
				tE.setRightPadding(band.getParent().getDefaultCellRightPadding());

				// if element is a border, move to the first position in the collection
				// it will have been created last, so that it could know the height of all previous items
				// but it needs to be rendered first so that it is "behind" other items
				if (ElementType.border.equals(type)) {
					band.getElements().add(0, tE);
				} else {
					band.getElements().add(tE);
				}

			}

		} catch (Exception e) {
			Util.LOGGER.warning("UNABLE TO CREATE REPORT ELEMENT IN BAND FOR " + name);
		}

		return band;
	}

	/**
	 * returns the name of the inverse condition get method - e.g. from notGood <--> isGood
	 * 
	 * @param conditionName
	 * @return
	 */
	public static String flipCondition(String conditionName) {
		String result = null;
		if (conditionName != null) {
			if (conditionName.startsWith("not")) {
				result = "is" + rawConditionName(conditionName).substring(0, 1).toUpperCase() + rawConditionName(conditionName).substring(1);
			} else {
				result = "not" + conditionName.substring(0, 1).toUpperCase() + conditionName.substring(1);
			}
		}
		return result;
	}

	public static String rawConditionName(String conditionName) {
		String result = conditionName;
		if (conditionName.startsWith("not")) {
			result = conditionName.substring(3, 4).toLowerCase() + conditionName.substring(4);
		} else {
			result = conditionName.substring(0, 1).toLowerCase() + conditionName.substring(1);
		}
		return result;
	}

	/**
	 * Generate the JRXML file - recursive call for all subreports
	 * 
	 * @param design
	 */
	public static void saveJrxml(DesignSpecification design, JasperReportRenderer reportRenderer) throws Exception {

		if (!design.getSubReports().isEmpty()) {
			for (DesignSpecification ds : design.getSubReports()) {
				saveJrxml(ds, reportRenderer);
			}
		}

		if (design.getRepositoryPath() == null) {
			throw new MetaDataException("ReportDesign has no repository path! Where do you want the files created?");
		}

		String moduleName = design.getModuleName();
		String documentName = design.getDocumentName();

		Path filePath = Paths.get(design.getRepositoryPath());
		if (design.getRepositoryPath() != null) {
			if (Boolean.TRUE.equals(design.getSaveToDocumentPackage())) {
				if (!filePath.endsWith("modules")) {
					filePath = filePath.resolve("modules");
				}

				filePath = filePath.resolve(moduleName).resolve(documentName).resolve("reports");

				// TODO - handle uxui options
				// if (uxui != null) {
				// filePath.append(uxui).append('/');
				// }
			} else {
				filePath = filePath.resolve("generatedReports");
			}

			if (!filePath.toFile().exists()) {
				filePath.toFile().mkdirs();
			}
			Path reportPath = filePath.resolve(design.getName() + ".jrxml");
			File file = reportPath.toFile();
			UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
			try (PrintWriter out = new PrintWriter(file)) {
				out.println(reportRenderer.getJrxml());
				out.flush();
			}
		}
	}

	/**
	 * Generate the JRXML file
	 * 
	 * @param design
	 */
	public static void saveJrxml(DesignSpecification design) throws Exception {
		Path filePath = Paths.get(design.getRepositoryPath());
		if (design.getRepositoryPath() != null) {
			if (Boolean.TRUE.equals(design.getSaveToDocumentPackage())) {
				if (!filePath.endsWith("modules")) {
					filePath = filePath.resolve("modules");
				}

				filePath = filePath.resolve(design.getModuleName()).resolve(design.getDocumentName()).resolve("reports");

				// TODO - handle uxui options
				// if (uxui != null) {
				// filePath.append(uxui).append('/');
				// }
			} else {
				filePath = filePath.resolve("generatedReports");
			}

			if (!filePath.toFile().exists()) {
				filePath.toFile().mkdirs();
			}
		}

		saveJrxml(design, filePath);
	}

	/**
	 * Generate the JRXML file - recursive call for all subreports
	 * 
	 * @param design
	 */
	public static void saveJrxml(DesignSpecification design, Path filePath) throws Exception {

		if (!design.getSubReports().isEmpty()) {
			for (DesignSpecification ds : design.getSubReports()) {
				saveJrxml(ds, filePath);
			}
		}

		if (design.getRepositoryPath() == null) {
			throw new MetaDataException("ReportDesign has no repository path! Where do you want the files created?");
		}

		if (design.getRepositoryPath() != null) {
			Path reportPath = filePath.resolve(design.getName() + ".jrxml");
			File file = reportPath.toFile();
			UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
			try (PrintWriter out = new PrintWriter(file)) {
				try {
					final JasperReportRenderer reportRenderer = new JasperReportRenderer(design);
					out.println(reportRenderer.getJrxml());
					out.flush();
				} catch (NullPointerException npe) {
					Util.LOGGER.warning(String.format("NullPointerException while writing report to %s", reportPath.toString()));
				}
			}
		}
	}

	/**
	 * Creates a method call expression for bound messages
	 * 
	 * @param design
	 * @param msg
	 * @return
	 */
	public static String renderBoundMessage(DesignSpecification design, String msg) {
		StringBuilder exp = new StringBuilder(64);
		exp.append("org.skyve.impl.generate.jasperreports.BeanForReport.getMessage(");
		if (Mode.bean.equals(design.getMode())) {
			exp.append("$F{THIS}").append(", ");
		} else {
			exp.append("\"").append(design.getModuleName()).append("\", ");
			exp.append("\"").append(design.getDocumentName()).append("\", ");
			exp.append("$P{ID}").append(", ");
		}
		exp.append("\"").append(msg).append("\")");

		return exp.toString();
	}
}
