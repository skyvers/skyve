package modules.admin.ReportDesign;

import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.ReportBand.SplitType;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementAlignment;
import org.skyve.impl.generate.jasperreports.ReportField;
import org.skyve.metadata.model.document.Collection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.ReportDesign;
import modules.admin.domain.ReportDesign.CollectionType;
import modules.admin.domain.ReportDesign.DefinitionSource;
import modules.admin.domain.ReportDesign.Mode;
import modules.admin.domain.ReportDesign.Orientation;
import modules.admin.domain.ReportDesign.ReportType;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient ReportDesignService reportDesignService;
 */
@Default
public class ReportDesignService {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReportDesignService.class);

	/**
	 * Copy fields from conceptual specification to this report design
	 * 
	 * @param result The ReportDesign instance to populate with values from the specification
	 * @param spec The DesignSpecification containing the source values to copy
	 * @return The populated ReportDesign instance
	 */
	@SuppressWarnings("static-method")
	public ReportDesign beanDesignFromSpecification(ReportDesign result, DesignSpecification spec) throws Exception {

		result.setName(spec.getName());
		if (spec.getMode() != null) {
			result.setMode(Mode.valueOf(spec.getMode().name()));
		}
		if (spec.getDefinitionSource() != null) {
			result.setDefinitionSource(DefinitionSource.valueOf(spec.getDefinitionSource().name()));
		}
		if (spec.getReportType() != null) {
			result.setReportType(ReportType.valueOf(spec.getReportType().name()));
			LOGGER.info("RESULT REPORT TYPE IS " + result.getReportType().toLocalisedDescription());
		}
		result.setModuleName(spec.getModuleName());
		result.setDocumentName(spec.getDocumentName());
		result.setRepositoryPath(spec.getRepositoryPath());
		result.setSaveToDocumentPackage(spec.getSaveToDocumentPackage());
		if (spec.getOrientation() != null) {
			result.setOrientation(Orientation.valueOf(spec.getOrientation().name()));
		}
		result.setWidth(spec.getWidth());
		result.setHeight(spec.getHeight());
		result.setLeftMargin(spec.getLeftMargin());
		result.setRightMargin(spec.getRightMargin());
		result.setTopMargin(spec.getTopMargin());
		result.setBottomMargin(spec.getBottomMargin());
		result.setColumnWidth(spec.getColumnWidth());
		result.setDefaultFontName(spec.getDefaultFontName());
		result.setTitleFontSize(spec.getTitleFontSize());
		result.setDefaultFontSize(spec.getDefaultFontSize());
		result.setRenderLabelAsTextFields(spec.getRenderLabelAsTextFields());
		result.setDefaultLineColour(spec.getDefaultLineColour());
		result.setDefaultLineWidth(spec.getDefaultLineWidth());
		result.setDefaultBorder(spec.getDefaultBorder());
		result.setDefaultBorderTop(spec.getDefaultBorderTop());
		result.setDefaultBorderLeft(spec.getDefaultBorderLeft());
		result.setDefaultBorderBottom(spec.getDefaultBorderBottom());
		result.setDefaultBorderRight(spec.getDefaultBorderRight());
		result.setDefaultElementHeight(spec.getDefaultElementHeight());
		result.setIncludePageNumbers(spec.getIncludePageNumbers());
		result.setDefaultCellTopPadding(spec.getDefaultCellTopPadding());
		result.setDefaultCellLeftPadding(spec.getDefaultCellLeftPadding());
		result.setDefaultCellBottomPadding(spec.getDefaultCellBottomPadding());
		result.setDefaultCellRightPadding(spec.getDefaultCellRightPadding());
		result.setDynamicFlow(spec.getDynamicFlow());
		if (spec.getBandSplitType() != null) {
			result.setBandSplitType(spec.getBandSplitType().toString());
		}
		result.setBoldLabels(spec.getBoldLabels());
		result.setCheckBoxFontName(spec.getCheckBoxFontName());
		result.setCheckBoxDisplayExpression(spec.getCheckBoxDisplayExpression());
		result.setPixelToTwip(spec.getPixelToTwip());
		result.setSectionBorderTop(spec.getSectionBorderTop());
		result.setSectionBorderLeft(spec.getSectionBorderLeft());
		result.setSectionBorderRight(spec.getSectionBorderRight());
		result.setSectionBorderBottom(spec.getSectionBorderBottom());
		result.setSectionTitleBorderTop(spec.getSectionTitleBorderTop());
		result.setSectionTitleBorderLeft(spec.getSectionTitleBorderLeft());
		result.setSectionTitleBorderRight(spec.getSectionTitleBorderRight());
		result.setSectionTitleBorderBottom(spec.getSectionTitleBorderBottom());
		result.setSectionTitleForeground(spec.getSectionTitleForeground());
		result.setSectionTitleBackground(spec.getSectionTitleBackground());
		if (spec.getField() != null) {
			result.setField(spec.getField().getDisplayName());
		}
		if (spec.getCollectionType() != null) {
			result.setCollectionType(CollectionType.valueOf(spec.getCollectionType().name()));
		}
		result.setParentReportPersistentName(spec.getParentReportPersistentName());
		result.setVerticalise(spec.getVerticalise());
		if (spec.getLabelAlignmentOverride() != null) {
			result.setLabelAlignmentOverride(spec.getLabelAlignmentOverride().toString());
		}

		return result;
	}

	/**
	 * Copy fields from ReportDesign bean to create a DesignSpecification
	 * 
	 * @param spec The ReportDesign instance containing the source values to copy
	 * @return A new DesignSpecification instance populated with values from the ReportDesign
	 */
	@SuppressWarnings("static-method")
	public DesignSpecification specificationFromDesignBean(ReportDesign spec) throws Exception {

		DesignSpecification result = new DesignSpecification();

		result.setName(spec.getName());
		if (spec.getMode() != null) {
			result.setMode(DesignSpecification.Mode.valueOf(spec.getMode().name()));
		}
		if (spec.getDefinitionSource() != null) {
			result.setDefinitionSource(DesignSpecification.DefinitionSource
					.valueOf(spec.getDefinitionSource().name()));
		}
		if (spec.getReportType() != null) {
			result.setReportType(
					DesignSpecification.ReportType.valueOf(spec.getReportType().name()));
			LOGGER.info("SPEC REPORT TYPE IS " + result.getReportType().toString());
		}
		result.setModuleName(spec.getModuleName());
		result.setDocumentName(spec.getDocumentName());
		result.setQueryName(spec.getQueryName());
		result.setRepositoryPath(spec.getRepositoryPath());
		result.setSaveToDocumentPackage(spec.getSaveToDocumentPackage());
		if (spec.getOrientation() != null) {
			result.setOrientation(
					DesignSpecification.Orientation.valueOf(spec.getOrientation().name()));
		}
		result.setWidth(spec.getWidth());
		result.setHeight(spec.getHeight());
		result.setLeftMargin(spec.getLeftMargin());
		result.setRightMargin(spec.getRightMargin());
		result.setTopMargin(spec.getTopMargin());
		result.setBottomMargin(spec.getBottomMargin());
		result.setColumnWidth(spec.getColumnWidth());
		result.setDefaultFontName(spec.getDefaultFontName());
		result.setTitleFontSize(spec.getTitleFontSize());
		result.setDefaultFontSize(spec.getDefaultFontSize());
		result.setRenderLabelAsTextFields(spec.getRenderLabelAsTextFields());
		result.setDefaultLineColour(spec.getDefaultLineColour());
		result.setDefaultLineWidth(spec.getDefaultLineWidth());
		result.setDefaultBorder(spec.getDefaultBorder());
		result.setDefaultBorderTop(spec.getDefaultBorderTop());
		result.setDefaultBorderLeft(spec.getDefaultBorderLeft());
		result.setDefaultBorderBottom(spec.getDefaultBorderBottom());
		result.setDefaultBorderRight(spec.getDefaultBorderRight());
		result.setDefaultElementHeight(spec.getDefaultElementHeight());
		result.setIncludePageNumbers(spec.getIncludePageNumbers());
		result.setDefaultCellTopPadding(spec.getDefaultCellTopPadding());
		result.setDefaultCellLeftPadding(spec.getDefaultCellLeftPadding());
		result.setDefaultCellBottomPadding(spec.getDefaultCellBottomPadding());
		result.setDefaultCellRightPadding(spec.getDefaultCellRightPadding());
		result.setDynamicFlow(spec.getDynamicFlow());
		if (spec.getBandSplitType() != null) {
			result.setBandSplitType(SplitType.valueOf(spec.getBandSplitType().toLowerCase()));
		}
		result.setBoldLabels(spec.getBoldLabels());
		result.setCheckBoxFontName(spec.getCheckBoxFontName());
		result.setCheckBoxDisplayExpression(spec.getCheckBoxDisplayExpression());
		result.setPixelToTwip(spec.getPixelToTwip());
		result.setSectionBorderTop(spec.getSectionBorderTop());
		result.setSectionBorderLeft(spec.getSectionBorderLeft());
		result.setSectionBorderRight(spec.getSectionBorderRight());
		result.setSectionBorderBottom(spec.getSectionBorderBottom());
		result.setSectionTitleBorderTop(spec.getSectionTitleBorderTop());
		result.setSectionTitleBorderLeft(spec.getSectionTitleBorderLeft());
		result.setSectionTitleBorderRight(spec.getSectionTitleBorderRight());
		result.setSectionTitleBorderBottom(spec.getSectionTitleBorderBottom());
		result.setSectionTitleForeground(spec.getSectionTitleForeground());
		result.setSectionTitleBackground(spec.getSectionTitleBackground());
		if (spec.getField() != null) {
			for (ReportField f : result.getFields()) {
				if (f.getName().equals(spec.getField())) {
					result.setField(f);
					break;
				}
			}

		}
		if (spec.getCollectionType() != null) {
			result.setCollectionType(
					Collection.CollectionType.valueOf(spec.getCollectionType().name()));
		}
		result.setParentReportPersistentName(spec.getParentReportPersistentName());
		result.setVerticalise(spec.getVerticalise());
		if (spec.getLabelAlignmentOverride() != null) {
			result.setLabelAlignmentOverride(ElementAlignment.valueOf(spec.getLabelAlignmentOverride()));
		}

		return result;
	}
}
