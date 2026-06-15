package modules.admin.ControlPanel;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.skyve.impl.util.UtilImpl;

import modules.admin.domain.ControlPanel;
import modules.admin.domain.ModuleDocument;

/**
 * Adds admin-only helper state used by Control Panel actions.
 */
public class ControlPanelExtension extends ControlPanel {
	private static final long serialVersionUID = -6204655500999983605L;

	private String unescapedResults;

	/**
	 * Create a new DocumentName to hold the module and document name for creating test data.
	 * 
	 * @param documentName The document name to add
	 * @return A {@link ModuleDocument} containing the module and document name for processing later
	 */
	public ModuleDocument addDocumentToCreate(final String documentName) {
		ModuleDocument docName = ModuleDocument.newInstance();
		docName.setModuleName(getTestModuleName());
		docName.setDocumentName(documentName);
		return docName;
	}
	
	/**
	 * Performs the trapException operation.
	 * @param e the e value
	 */
	public void trapException(Exception e) {
		StringWriter sw = new StringWriter(512);
		PrintWriter pw = new PrintWriter(sw);
		e.printStackTrace(pw);
		setResults(sw.toString());
	}

	/**
	 * Overriden to escape {, < & >.
	 * Add a new line out the front to line up all lines to the left of the blurb.
	 */
	@Override
	public void setResults(String results) {
		setResults(results, true);
	}
	
	/**
	 * Stores query/action output and optionally escapes characters that interfere with
	 * XML/markup rendering.
	 *
	 * @param results The textual output to store.
	 * @param escape Whether to escape markup-sensitive characters before storing.
	 */
	public void setResults(String results, boolean escape) {
		unescapedResults = results;
		if ((escape) && (results != null)) {
			super.setResults('\n' + results.replace("{", "\\{").replace("<", "&lt;").replace(">", "&gt;"));
		}
		else {
			super.setResults(results);
		}
	}

	/**
	 * Returns the last result payload before any escaping or formatting is applied.
	 *
	 * @return The unescaped result payload.
	 */
	public String getUnescapedResults() {
		return unescapedResults;
	}

	/**
	 * Returns whether Bizlet tracing is currently enabled.
	 *
	 * @return {@code true} when Bizlet tracing is enabled.
	 */
	@Override
	public Boolean getBizletTrace() {
		return Boolean.valueOf(UtilImpl.BIZLET_TRACE);
	}

	/**
	 * Enables or disables Bizlet tracing.
	 *
	 * @param bizletTrace The new Bizlet trace flag.
	 */
	@Override
	public void setBizletTrace(Boolean bizletTrace) {
		UtilImpl.BIZLET_TRACE = Boolean.TRUE.equals(bizletTrace);
	}

	/**
	 * Returns whether command tracing is currently enabled.
	 *
	 * @return {@code true} when command tracing is enabled.
	 */
	@Override
	public Boolean getCommandTrace() {
		return Boolean.valueOf(UtilImpl.COMMAND_TRACE);
	}

	/**
	 * Enables or disables command tracing.
	 *
	 * @param commandTrace The new command trace flag.
	 */
	@Override
	public void setCommandTrace(Boolean commandTrace) {
		UtilImpl.COMMAND_TRACE = Boolean.TRUE.equals(commandTrace);
	}

	/**
	 * Returns whether content tracing is currently enabled.
	 *
	 * @return {@code true} when content tracing is enabled.
	 */
	@Override
	public Boolean getContentTrace() {
		return Boolean.valueOf(UtilImpl.CONTENT_TRACE);
	}

	/**
	 * Enables or disables content tracing.
	 *
	 * @param contentTrace The new content trace flag.
	 */
	@Override
	public void setContentTrace(Boolean contentTrace) {
		UtilImpl.CONTENT_TRACE = Boolean.TRUE.equals(contentTrace);
	}

	/**
	 * Returns whether dirty-state tracing is currently enabled.
	 *
	 * @return {@code true} when dirty-state tracing is enabled.
	 */
	@Override
	public Boolean getDirtyTrace() {
		return Boolean.valueOf(UtilImpl.DIRTY_TRACE);
	}

	/**
	 * Enables or disables dirty-state tracing.
	 *
	 * @param dirtyTrace The new dirty-state trace flag.
	 */
	@Override
	public void setDirtyTrace(Boolean dirtyTrace) {
		UtilImpl.DIRTY_TRACE = Boolean.TRUE.equals(dirtyTrace);
	}

	/**
	 * Returns whether Faces lifecycle tracing is currently enabled.
	 *
	 * @return {@code true} when Faces lifecycle tracing is enabled.
	 */
	@Override
	public Boolean getFacesTrace() {
		return Boolean.valueOf(UtilImpl.FACES_TRACE);
	}

	/**
	 * Enables or disables Faces lifecycle tracing.
	 *
	 * @param facesTrace The new Faces trace flag.
	 */
	@Override
	public void setFacesTrace(Boolean facesTrace) {
		UtilImpl.FACES_TRACE = Boolean.TRUE.equals(facesTrace);
	}

	/**
	 * Returns whether HTTP tracing is currently enabled.
	 *
	 * @return {@code true} when HTTP tracing is enabled.
	 */
	@Override
	public Boolean getHttpTrace() {
		return Boolean.valueOf(UtilImpl.HTTP_TRACE);
	}

	/**
	 * Enables or disables HTTP tracing.
	 *
	 * @param httpTrace The new HTTP trace flag.
	 */
	@Override
	public void setHttpTrace(Boolean httpTrace) {
		UtilImpl.HTTP_TRACE = Boolean.TRUE.equals(httpTrace);
	}

	/*
	 * Cant influence this setting as it is set in the hibernate session factory
	 * 
	 * @Override
	 * public Boolean getPrettySqlOutput() {
	 * return Boolean.valueOf(UtilImpl.PRETTY_SQL_OUTPUT);
	 * }
	 * 
	 * @Override
	 * public void setPrettySqlOutput(Boolean prettySqlOutput) {
	 * UtilImpl.PRETTY_SQL_OUTPUT = Boolean.TRUE.equals(prettySqlOutput);
	 * }
	 */
	@Override
	public Boolean getQueryTrace() {
		return Boolean.valueOf(UtilImpl.QUERY_TRACE);
	}

	@Override
	public void setQueryTrace(Boolean queryTrace) {
		UtilImpl.QUERY_TRACE = Boolean.TRUE.equals(queryTrace);
	}

	/**
	 * Returns whether security tracing is currently enabled.
	 *
	 * @return {@code true} when security tracing is enabled.
	 */
	@Override
	public Boolean getSecurityTrace() {
		return Boolean.valueOf(UtilImpl.SECURITY_TRACE);
	}

	/**
	 * Enables or disables security tracing.
	 *
	 * @param securityTrace The new security trace flag.
	 */
	@Override
	public void setSecurityTrace(Boolean securityTrace) {
		UtilImpl.SECURITY_TRACE = Boolean.TRUE.equals(securityTrace);
	}

	/*
	 * Cant influence this setting as it is set in the hibernate session factory
	 * 
	 * @Override
	 * public Boolean getSqlTrace() {
	 * return Boolean.valueOf(UtilImpl.SQL_TRACE);
	 * }
	 * 
	 * @Override
	 * public void setSqlTrace(Boolean sqlTrace) {
	 * UtilImpl.SQL_TRACE = Boolean.TRUE.equals(sqlTrace);
	 * }
	 */
	@Override
	public Boolean getXmlTrace() {
		return Boolean.valueOf(UtilImpl.XML_TRACE);
	}

	@Override
	public void setXmlTrace(Boolean xmlTrace) {
		UtilImpl.XML_TRACE = Boolean.TRUE.equals(xmlTrace);
	}
}
