# runMatlab
# functions related to connecting/running matlab
library(R.matlab)

runMatlab <- function(port=9998, attempt=1){
	# launches a matlab server, trys to connect for up to 30 seconds
	# if it fails it will try a different port
	# failed connections can take a long time due to the ~30 second possible connection time
	out <- tryCatch({
		# try starting a server
		Matlab$startServer(
			matlab="/usr/local/MATLAB/R2019a/bin/matlab",
			workdir='/srv/matlab-working-dir',
			port=port
	    )
		matlab <- Matlab(port=port)

		# for debugging R.Matlab connection
		# set to -2 for max verbosity
		#setVerbose(matlab, threshold = 0)

		isOpen <- open(matlab)

		if (!(isOpen)){
			throw("ERROR MATLAB server is not running: waited 30 seconds.")
		}else{
			message("--------------------------------------------------------------------------------")
			message("matlab is open: ", matlab)
			message("--------------------------------------------------------------------------------")

			#----------------------------------------
			# Required Functions
			#----------------------------------------
			mat_filepath <- file.path("/media/SamMATLAB", "Filesfrom_05062019mtg.mat");
			data <- readMat(mat_filepath)


			# things referenced in MutliRank.m

			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_idx.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "MultiRank_bydam_prefUnityCheck.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "MultiRank_bydam_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_netOV_split_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_fitfn_split.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_fitfn_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## (requires DamIndex to be set)
			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPF_netOV_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#m_filepath <- file.path("/media/SamMATLAB", "DPPFwkshp_prep_minimum.m");
			#m_file_as_str <- readChar(m_filepath, file.info(m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			#----------------------------------------
			# Application Entrypoint "Instructions"
			#----------------------------------------

			#base_m_filepath <- file.path("/media/SamMATLAB", "INSTRUCTIONS_minimum.m");
			#m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			## "instruction file" has been split into multiple files
			#base_m_filepath <- file.path("/media/SamMATLAB", "INSTRUCTIONS_minimum2.m");
			#m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			#setFunction(matlab, m_file_as_str)

			base_m_filepath <- file.path("/media/SamMATLAB", "MultiRank.m");
			m_file_as_str <- readChar(base_m_filepath, file.info(base_m_filepath)$size)
			setFunction(matlab, m_file_as_str)

			eval_string = sprintf("idxRank=MultiRank(f,pref,varargin);")
			evaluate(matlab, eval_string)
			close(matlab)

			#eval_string = sprintf( "[DamIndex, x, out]=RunApplication2();")
			#evaluate(matlab, eval_string)
			#close(matlab)

			message("--------------------------------------------------------------------------------")
			message("matlab closed")
			message("--------------------------------------------------------------------------------")
			return(TRUE) # sucess
		}
	},
	error=function(cond){
		message("--------------------------------------------------------------------------------")
		message("runMatlab Error", cond)

		if (isOpen){
			message("Error but matlab Open, close it.")
			close(matlab)
		}

		if ( retry_matlab_connection && (attempt < max_retries) ){

			# server already open, assume someone else is using
			session_matlab_port <- (port -1)
			message("try running on another port")
			message("attempts: ", attempt)
			message("max possible attempts: ", max_retries)
			message("--------------------------------------------------------------------------------")
			return( runMatlab(port=(port-1), attempt=(attempt+1)) )

		}else{

			message("failing execution after max tries")
			message("--------------------------------------------------------------------------------")
			return(FALSE)
		}
	},
	warning=function(cond){
		message("--------------------------------------------------------------------------------")
		message("runMatlab Warning", cond)
		message("--------------------------------------------------------------------------------")
		return(FALSE) # fail?
	})

	message("attempt ", attempt)
	message("runMatlab RESULTS: success?:", out)
	return(out)
}
