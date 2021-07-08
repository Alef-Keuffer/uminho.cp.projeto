--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.NV
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing NV extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.NV (
  module Graphics.GL.NV.AlphaToCoverageDitherControl,
  module Graphics.GL.NV.BindlessMultiDrawIndirect,
  module Graphics.GL.NV.BindlessMultiDrawIndirectCount,
  module Graphics.GL.NV.BindlessTexture,
  module Graphics.GL.NV.BlendEquationAdvanced,
  module Graphics.GL.NV.BlendEquationAdvancedCoherent,
  module Graphics.GL.NV.BlendMinmaxFactor,
  module Graphics.GL.NV.ClipSpaceWScaling,
  module Graphics.GL.NV.CommandList,
  module Graphics.GL.NV.ComputeProgram5,
  module Graphics.GL.NV.ConditionalRender,
  module Graphics.GL.NV.ConservativeRaster,
  module Graphics.GL.NV.ConservativeRasterDilate,
  module Graphics.GL.NV.ConservativeRasterPreSnap,
  module Graphics.GL.NV.ConservativeRasterPreSnapTriangles,
  module Graphics.GL.NV.CopyDepthToColor,
  module Graphics.GL.NV.CopyImage,
  module Graphics.GL.NV.DeepTexture3D,
  module Graphics.GL.NV.DepthBufferFloat,
  module Graphics.GL.NV.DepthClamp,
  module Graphics.GL.NV.DrawTexture,
  module Graphics.GL.NV.DrawVulkanImage,
  module Graphics.GL.NV.Evaluators,
  module Graphics.GL.NV.ExplicitMultisample,
  module Graphics.GL.NV.Fence,
  module Graphics.GL.NV.FillRectangle,
  module Graphics.GL.NV.FloatBuffer,
  module Graphics.GL.NV.FogDistance,
  module Graphics.GL.NV.FragmentCoverageToColor,
  module Graphics.GL.NV.FragmentProgram,
  module Graphics.GL.NV.FragmentProgram2,
  module Graphics.GL.NV.FramebufferMixedSamples,
  module Graphics.GL.NV.FramebufferMultisampleCoverage,
  module Graphics.GL.NV.GPUMulticast,
  module Graphics.GL.NV.GPUProgram4,
  module Graphics.GL.NV.GPUProgram5,
  module Graphics.GL.NV.GPUShader5,
  module Graphics.GL.NV.GeometryProgram4,
  module Graphics.GL.NV.HalfFloat,
  module Graphics.GL.NV.InternalformatSampleQuery,
  module Graphics.GL.NV.LightMaxExponent,
  module Graphics.GL.NV.MemoryAttachment,
  module Graphics.GL.NV.MeshShader,
  module Graphics.GL.NV.MultisampleCoverage,
  module Graphics.GL.NV.MultisampleFilterHint,
  module Graphics.GL.NV.OcclusionQuery,
  module Graphics.GL.NV.PackedDepthStencil,
  module Graphics.GL.NV.ParameterBufferObject,
  module Graphics.GL.NV.PathRenderingCompatibility,
  module Graphics.GL.NV.PathRenderingCore,
  module Graphics.GL.NV.PathRenderingSharedEdge,
  module Graphics.GL.NV.PixelDataRange,
  module Graphics.GL.NV.PointSprite,
  module Graphics.GL.NV.PresentVideo,
  module Graphics.GL.NV.PrimitiveRestart,
  module Graphics.GL.NV.QueryResource,
  module Graphics.GL.NV.QueryResourceTag,
  module Graphics.GL.NV.RegisterCombiners,
  module Graphics.GL.NV.RegisterCombiners2,
  module Graphics.GL.NV.RepresentativeFragmentTest,
  module Graphics.GL.NV.RobustnessVideoMemoryPurge,
  module Graphics.GL.NV.SampleLocations,
  module Graphics.GL.NV.ScissorExclusive,
  module Graphics.GL.NV.ShaderBufferLoad,
  module Graphics.GL.NV.ShaderBufferStore,
  module Graphics.GL.NV.ShaderSubgroupPartitioned,
  module Graphics.GL.NV.ShaderThreadGroup,
  module Graphics.GL.NV.ShadingRateImage,
  module Graphics.GL.NV.TessellationProgram5,
  module Graphics.GL.NV.TexgenEmboss,
  module Graphics.GL.NV.TexgenReflection,
  module Graphics.GL.NV.TextureBarrier,
  module Graphics.GL.NV.TextureEnvCombine4,
  module Graphics.GL.NV.TextureExpandNormal,
  module Graphics.GL.NV.TextureMultisample,
  module Graphics.GL.NV.TextureRectangle,
  module Graphics.GL.NV.TextureShader,
  module Graphics.GL.NV.TextureShader2,
  module Graphics.GL.NV.TextureShader3,
  module Graphics.GL.NV.TransformFeedback,
  module Graphics.GL.NV.TransformFeedback2,
  module Graphics.GL.NV.UniformBufferUnifiedMemory,
  module Graphics.GL.NV.VDPAUInterop,
  module Graphics.GL.NV.VDPAUInterop2,
  module Graphics.GL.NV.VertexArrayRange,
  module Graphics.GL.NV.VertexArrayRange2,
  module Graphics.GL.NV.VertexAttribInteger64Bit,
  module Graphics.GL.NV.VertexBufferUnifiedMemory,
  module Graphics.GL.NV.VertexProgram,
  module Graphics.GL.NV.VertexProgram2Option,
  module Graphics.GL.NV.VertexProgram3,
  module Graphics.GL.NV.VertexProgram4,
  module Graphics.GL.NV.VideoCapture,
  module Graphics.GL.NV.ViewportSwizzle
) where

import Graphics.GL.NV.AlphaToCoverageDitherControl
import Graphics.GL.NV.BindlessMultiDrawIndirect
import Graphics.GL.NV.BindlessMultiDrawIndirectCount
import Graphics.GL.NV.BindlessTexture
import Graphics.GL.NV.BlendEquationAdvanced
import Graphics.GL.NV.BlendEquationAdvancedCoherent
import Graphics.GL.NV.BlendMinmaxFactor
import Graphics.GL.NV.ClipSpaceWScaling
import Graphics.GL.NV.CommandList
import Graphics.GL.NV.ComputeProgram5
import Graphics.GL.NV.ConditionalRender
import Graphics.GL.NV.ConservativeRaster
import Graphics.GL.NV.ConservativeRasterDilate
import Graphics.GL.NV.ConservativeRasterPreSnap
import Graphics.GL.NV.ConservativeRasterPreSnapTriangles
import Graphics.GL.NV.CopyDepthToColor
import Graphics.GL.NV.CopyImage
import Graphics.GL.NV.DeepTexture3D
import Graphics.GL.NV.DepthBufferFloat
import Graphics.GL.NV.DepthClamp
import Graphics.GL.NV.DrawTexture
import Graphics.GL.NV.DrawVulkanImage
import Graphics.GL.NV.Evaluators
import Graphics.GL.NV.ExplicitMultisample
import Graphics.GL.NV.Fence
import Graphics.GL.NV.FillRectangle
import Graphics.GL.NV.FloatBuffer
import Graphics.GL.NV.FogDistance
import Graphics.GL.NV.FragmentCoverageToColor
import Graphics.GL.NV.FragmentProgram
import Graphics.GL.NV.FragmentProgram2
import Graphics.GL.NV.FramebufferMixedSamples
import Graphics.GL.NV.FramebufferMultisampleCoverage
import Graphics.GL.NV.GPUMulticast
import Graphics.GL.NV.GPUProgram4
import Graphics.GL.NV.GPUProgram5
import Graphics.GL.NV.GPUShader5
import Graphics.GL.NV.GeometryProgram4
import Graphics.GL.NV.HalfFloat
import Graphics.GL.NV.InternalformatSampleQuery
import Graphics.GL.NV.LightMaxExponent
import Graphics.GL.NV.MemoryAttachment
import Graphics.GL.NV.MeshShader
import Graphics.GL.NV.MultisampleCoverage
import Graphics.GL.NV.MultisampleFilterHint
import Graphics.GL.NV.OcclusionQuery
import Graphics.GL.NV.PackedDepthStencil
import Graphics.GL.NV.ParameterBufferObject
import Graphics.GL.NV.PathRenderingCompatibility
import Graphics.GL.NV.PathRenderingCore
import Graphics.GL.NV.PathRenderingSharedEdge
import Graphics.GL.NV.PixelDataRange
import Graphics.GL.NV.PointSprite
import Graphics.GL.NV.PresentVideo
import Graphics.GL.NV.PrimitiveRestart
import Graphics.GL.NV.QueryResource
import Graphics.GL.NV.QueryResourceTag
import Graphics.GL.NV.RegisterCombiners
import Graphics.GL.NV.RegisterCombiners2
import Graphics.GL.NV.RepresentativeFragmentTest
import Graphics.GL.NV.RobustnessVideoMemoryPurge
import Graphics.GL.NV.SampleLocations
import Graphics.GL.NV.ScissorExclusive
import Graphics.GL.NV.ShaderBufferLoad
import Graphics.GL.NV.ShaderBufferStore
import Graphics.GL.NV.ShaderSubgroupPartitioned
import Graphics.GL.NV.ShaderThreadGroup
import Graphics.GL.NV.ShadingRateImage
import Graphics.GL.NV.TessellationProgram5
import Graphics.GL.NV.TexgenEmboss
import Graphics.GL.NV.TexgenReflection
import Graphics.GL.NV.TextureBarrier
import Graphics.GL.NV.TextureEnvCombine4
import Graphics.GL.NV.TextureExpandNormal
import Graphics.GL.NV.TextureMultisample
import Graphics.GL.NV.TextureRectangle
import Graphics.GL.NV.TextureShader
import Graphics.GL.NV.TextureShader2
import Graphics.GL.NV.TextureShader3
import Graphics.GL.NV.TransformFeedback
import Graphics.GL.NV.TransformFeedback2
import Graphics.GL.NV.UniformBufferUnifiedMemory
import Graphics.GL.NV.VDPAUInterop
import Graphics.GL.NV.VDPAUInterop2
import Graphics.GL.NV.VertexArrayRange
import Graphics.GL.NV.VertexArrayRange2
import Graphics.GL.NV.VertexAttribInteger64Bit
import Graphics.GL.NV.VertexBufferUnifiedMemory
import Graphics.GL.NV.VertexProgram
import Graphics.GL.NV.VertexProgram2Option
import Graphics.GL.NV.VertexProgram3
import Graphics.GL.NV.VertexProgram4
import Graphics.GL.NV.VideoCapture
import Graphics.GL.NV.ViewportSwizzle