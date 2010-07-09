package gov.nih.nci.caintegrator.analysis.server;

import gov.nih.nci.caintegrator.analysis.messaging.AnalysisRequest;
import gov.nih.nci.caintegrator.analysis.messaging.AnalysisResult;
import gov.nih.nci.caintegrator.analysis.messaging.HeatMapRequest;
import gov.nih.nci.caintegrator.analysis.messaging.HeatMapResult;
import gov.nih.nci.caintegrator.analysis.messaging.ReporterGroup;
import gov.nih.nci.caintegrator.analysis.messaging.SampleGroup;
import gov.nih.nci.caintegrator.exceptions.AnalysisServerException;

import org.apache.log4j.Logger;
import org.rosuda.REngine.Rserve.RFileInputStream;

public class HeatMapTaskR extends AnalysisTaskR {

	private static Logger logger = Logger.getLogger(HeatMapTaskR.class);
	private HeatMapResult heatMapResult;
	
	public HeatMapTaskR(AnalysisRequest request, boolean debugRcommands) {
		super(request, debugRcommands);
	}

	@Override
	public void run() {
		HeatMapRequest request = (HeatMapRequest)getRequest();
		SampleGroup sampleGroup = request.getSampleGroup();
		ReporterGroup reporterGroup = request.getReporterGroup();
		
		try {
			
			String dataFileName = request.getDataFileName();
			if (dataFileName != null) {
			  setDataFile(request.getDataFileName());
			}
			else {
			  throw new AnalysisServerException("Null data file name");
			}
			
			String baseFileName = "heatmap_" + getRequest().getSessionId() + "_" + System.currentTimeMillis();
			String rCmd = "";
			doRvoidEval("hcInputMatrix <- dataMatrix");
			if(sampleGroup != null) {
				rCmd = getRgroupCmd("sampleIds", sampleGroup);
				doRvoidEval(rCmd);
				rCmd = "hcInputMatrix <- getSubmatrix.onegrp(hcInputMatrix, sampleIds)";
				doRvoidEval(rCmd);
			}
			
			if (reporterGroup != null) {
				rCmd = getRgroupCmd("reporterIds", reporterGroup);
				doRvoidEval(rCmd);
				rCmd = "hcInputMatrix <- getSubmatrix.rep(hcInputMatrix, reporterIds)";
				doRvoidEval(rCmd);
			}
			
			rCmd = "dat<-na.omit(hcInputMatrix)";
			doRvoidEval(rCmd);
			rCmd = "scale.r<-scale(dat,center=TRUE,scale=TRUE)";
			doRvoidEval(rCmd);
			rCmd = "library(ctc)";
			doRvoidEval(rCmd);
			rCmd = "hclust2treeview(scale.r,file=\"" + baseFileName + ".cdt\",method = \"euclidean\",link = \"ward\",keep.hclust=T)";
			doRvoidEval(rCmd);
			
			RFileInputStream is = getRComputeConnection().openFile(baseFileName + ".cdt");
			byte[] cdtFile = getBytes(is);
			is.close();
			
			is = getRComputeConnection().openFile(baseFileName + ".atr");
			byte[] atrFile = getBytes(is);
			is.close();
			
			
			is = getRComputeConnection().openFile(baseFileName + ".gtr");
			byte[] gtrFile = getBytes(is);
			is.close();
			
			heatMapResult = new HeatMapResult(getRequest().getSessionId(), getRequest().getTaskId());
			heatMapResult.setCdtFile(cdtFile);
			heatMapResult.setAtrFile(atrFile);
			heatMapResult.setGtrFile(gtrFile);
			
			getRComputeConnection().removeFile(baseFileName + ".cdt");
			getRComputeConnection().removeFile(baseFileName + ".atr");
			getRComputeConnection().removeFile(baseFileName + ".gtr");
			
			
		} catch (AnalysisServerException e) {
			e.setFailedRequest(request);
			logger.error("Internal Error. " + e.getMessage());
			logStackTrace(logger, e);
			setException(e);
			return;
		} catch (Exception ex) {
			AnalysisServerException asex = new AnalysisServerException(
					"Internal Error. Caught AnalysisServerException in HeatMapTaskR." + ex.getMessage());
			asex.setFailedRequest(request);
			setException(asex);	        
			logStackTrace(logger, ex);
			return;  
		}

	}

	@Override
	public void cleanUp() {
		try {
			setRComputeConnection(null);
		} catch (AnalysisServerException e) {
			logger.error("Error in cleanUp method");
			logStackTrace(logger, e);
		    setException(e);
		}
	}

	@Override
	public AnalysisResult getResult() {
		return heatMapResult;
	}

}
