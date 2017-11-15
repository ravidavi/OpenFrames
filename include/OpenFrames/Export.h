/***********************************
   Copyright 2017 Ravishankar Mathur

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
***********************************/

#ifndef _OF_EXPORT_
#define _OF_EXPORT_

#if defined(_MSC_VER)
  #pragma warning( disable : 4244 )
  #pragma warning( disable : 4251 )
  #pragma warning( disable : 4267 )
  #pragma warning( disable : 4275 )
  #pragma warning( disable : 4290 )
  #pragma warning( disable : 4786 )
  #pragma warning( disable : 4305 )
#endif

#if defined(_MSC_VER) || defined(__CYGWIN__) || defined(__MINGW32__) || defined( __BCPLUSPLUS__)  || defined( __MWERKS__)
  #  ifdef OF_LIBRARY
    #  define OF_EXPORT   __declspec(dllexport)
  #  else
    #  define OF_EXPORT   __declspec(dllimport)
  #  endif /* OF_LIBRARY */
#else
  #  define OF_EXPORT
#endif  

// set up define for whether member templates are supported by VisualStudio compilers.
#ifdef _MSC_VER
# if (_MSC_VER >= 1300)
#  define __STL_MEMBER_TEMPLATES
# endif
#endif

#endif
